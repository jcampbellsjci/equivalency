#### G-League Equivalency ####

# Loading up necessary packages
library(rvest)
library(Metrics)
library(tidyverse)

# Going to use map to loop through urls and pull g-leage data
g_league <- map(.x = 2015:2019,
                .f = ~ read_html(paste(
                  # "https://www.basketball-reference.com/gleague/years/gleague_",
                  # .x, "_per_game.html", sep = "")) %>%
                  "https://www.basketball-reference.com/gleague/years/gleague_",
                  .x, "_per_minute.html", sep = "")) %>%
                  # html_nodes('#per_game_stats') %>%
                  html_nodes('#per_minute_stats') %>%
                  html_table() %>%
                  .[[1]] %>%
                  # Filtering out any header rows
                  filter(Tm != "Tm") %>%
                  mutate_all(na_if, "") %>%
                  mutate_at(.vars = vars(Age:PTS),
                            .funs = list(as.numeric)) %>%
                  distinct(Player, .keep_all = T)
                )
# Output is in a list
# Name each element of the list to use as an ID
names(g_league) <- 2015:2019
g_league <- g_league %>%
  bind_rows(.id = "Season")

# Performing looping for same timeframe for NBA data
nba <- map(.x = 2015:2019,
                .f = ~ read_html(paste(
                  # "https://www.basketball-reference.com/leagues/NBA_",
                  # .x, "_per_game.html", sep = "")) %>%
                  "https://www.basketball-reference.com/leagues/NBA_",
                  .x, "_per_minute.html", sep = "")) %>%
                  # html_nodes('#per_game_stats') %>%
                  html_nodes('#per_minute_stats') %>%
                  html_table() %>%
                  .[[1]] %>%
                  filter(Tm != "Tm") %>%
                  mutate_all(na_if, "") %>%
                  mutate_at(.vars = vars(Age, G:PTS), 
                            .funs = list(as.numeric)) %>%
                  distinct(Player, .keep_all = T)
)
names(nba) <- 2015:2019
nba <- nba %>%
  bind_rows(.id = "Season")

# Combine both the NBA and G-League data
combo <- nba %>%
  inner_join(g_league, by = c("Player", "Season")) %>%
  # Any variable on the left table will be listed as NBA
  rename_at(.vars = vars(contains(".x")), 
            .funs = funs(sub(".x", "_nba", .))) %>%
  # Any variable on the right table will be listed as G-League
  rename_at(.vars = vars(contains(".y")), 
            .funs = funs(sub(".y", "_gleague", .))) %>%
  # Filtering specifically for players who played at least 10 games
  # in each league in a season
  filter(G_nba >= 10 & G_gleague >= 10)

# Calculating general ratios for stats
# Shows how much a G-League stat is worth in the NBA
ratios <- combo %>%
  mutate(PTS_RATIO = PTS_nba / PTS_gleague,
         AST_RATIO = AST_nba / AST_gleague,
         REB_RATIO = TRB_nba / TRB_gleague) %>%
  select(Season, Player:Tm_nba, Tm_gleague, PTS_RATIO:REB_RATIO) %>%
  summarize_at(.vars = vars(PTS_RATIO:REB_RATIO),
               .funs = list(mean))

# Finding general NBA equivalent stats for all G-League players
g_league_equiv <- g_league %>%
  mutate(PTS_EQUIV = PTS * ratios$PTS_RATIO,
         AST_EQUIV = AST * ratios$AST_RATIO,
         REB_EQUIV = TRB * ratios$REB_RATIO) %>%
  select(Season, Player, PTS_EQUIV:REB_EQUIV)

# Measuring accuracy of this generalized method
accuracy_df <- combo %>%
  select(Player, Season, PTS_nba, AST_nba, TRB_nba) %>%
  inner_join(g_league_equiv, by = c("Player", "Season")) %>%
  # Calculating RMSE for all players
  summarize(PTS_accuracy = rmse(actual = PTS_nba,
                                predicted = PTS_EQUIV),
            AST_accuracy = rmse(actual = AST_nba,
                                predicted = AST_EQUIV),
            REB_accuracy = rmse(actual = TRB_nba,
                                predicted = REB_EQUIV))
