library(rvest)
library(tidyverse)

read_html("https://www.basketball-reference.com/gleague/years/gleague_2019_per_game.html") %>%
  html_nodes('#per_game_stats') %>%
  html_table() %>%
  .[[1]] %>%
  filter(Tm != "Tm") %>%
  mutate_all(na_if, "") %>%
  mutate_at(.vars = vars(Age:PTS), .funs = list(as.numeric))

g_league <- map(.x = 2015:2019,
                .f = ~ read_html(paste(
                  "https://www.basketball-reference.com/gleague/years/gleague_",
                  .x, "_per_game.html", sep = "")) %>%
                  html_nodes('#per_game_stats') %>%
                  html_table() %>%
                  .[[1]] %>%
                  filter(Tm != "Tm") %>%
                  mutate_all(na_if, "") %>%
                  mutate_at(.vars = vars(Age:PTS), .funs = list(as.numeric)) %>%
                  distinct(Player, .keep_all = T)
                )
names(g_league) <- 2015:2019
g_league <- g_league %>%
  bind_rows(.id = "Season")

nba <- map(.x = 2015:2019,
                .f = ~ read_html(paste(
                  "https://www.basketball-reference.com/leagues/NBA_",
                  .x, "_per_game.html", sep = "")) %>%
                  html_nodes('#per_game_stats') %>%
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

combo <- nba %>%
  inner_join(g_league, by = c("Player", "Season")) %>%
  rename_at(.vars = vars(contains(".x")), 
            .funs = funs(sub(".x", "_nba", .))) %>%
  rename_at(.vars = vars(contains(".y")), 
            .funs = funs(sub(".y", "_gleague", .))) %>%
  filter(G_nba >= 10 & G_gleague >= 10)

ratios <- combo %>%
  mutate(PTS_RATIO = PTS_nba / PTS_gleague,
         AST_RATIO = AST_nba / AST_gleague,
         REB_RATIO = TRB_nba / TRB_gleague) %>%
  select(Season, Player:Tm_nba, Tm_gleague, PTS_RATIO:REB_RATIO) %>%
  summarize_at(.vars = vars(PTS_RATIO:REB_RATIO),
               .funs = list(mean))

g_league_dist <- g_league %>%
  filter(!(paste(Player, Season) %in% paste(combo$Player, combo$Season))) %>%
  

g_league_equiv <- g_league %>%
  filter(!(paste(Player, Season) %in% paste(combo$Player, combo$Season))) %>%
  mutate(PTS_EQUIV = PTS * ratios$PTS_RATIO,
         AST_EQUIV = AST * ratios$AST_RATIO,
         REB_EQUIV = TRB * ratios$REB_RATIO) %>%
  select(Season, Player, PTS_EQUIV:REB_EQUIV)
