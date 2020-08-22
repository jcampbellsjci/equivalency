library(rvest)
library(tidyverse)


#### Pulling Data ####

# Going to use map to loop through urls and pull g-leage data
g_league <- map(.x = 2016:2020,
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
names(g_league) <- 2016:2020
g_league <- g_league %>%
  bind_rows(.id = "Season")

# Performing looping for same timeframe for NBA data
nba <- map(.x = 2016:2020,
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
names(nba) <- 2016:2020
nba <- nba %>%
  bind_rows(.id = "Season")

# Combine both the NBA and G-League data
combo <- nba %>%
  inner_join(g_league, by = c("Player", "Season")) %>%
  # Any variable on the left table will be listed as NBA
  rename_at(.vars = vars(contains(".x")), 
            .funs = list(~ sub(".x", "_nba", .))) %>%
  # Any variable on the right table will be listed as G-League
  rename_at(.vars = vars(contains(".y")), 
            .funs = list(~ sub(".y", "_gleague", .))) %>%
  # Filtering specifically for players who played at least 10 games
  # in each league in a season
  filter(G_nba >= 10 & G_gleague >= 10)

# Writing data to folder
write_csv(x = combo, path = "data/combo.csv")
write_csv(x = g_league, path = "data/g_league.csv")
write_csv(x = nba, path = "data/nba.csv")
