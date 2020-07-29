library(yardstick)
library(tidyverse)

combo <- read_csv("data/combo.csv")


#### Basic Equivalency ####

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
map2_df(.x = combo %>%
          mutate(Season = as.character(Season)) %>%
          inner_join(g_league_equiv, by = c("Player", "Season")) %>%
          select(PTS_nba, AST_nba, TRB_nba),
        .y = combo %>%
          mutate(Season = as.character(Season)) %>%
          inner_join(g_league_equiv, by = c("Player", "Season")) %>%
          select(PTS_EQUIV, AST_EQUIV, REB_EQUIV),
        .f = ~ rmse_vec(truth = .x, estimate = .y))
