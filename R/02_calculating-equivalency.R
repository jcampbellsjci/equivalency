library(ModelMetrics)
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
accuracy_df <- combo %>%
  select(Player, Season, PTS_nba, AST_nba, TRB_nba) %>%
  mutate(Season = as.character(Season)) %>%
  inner_join(g_league_equiv, by = c("Player", "Season")) %>%
  # Calculating RMSE for all players
  summarize(PTS_accuracy = rmse(actual = PTS_nba,
                                predicted = PTS_EQUIV),
            AST_accuracy = rmse(actual = AST_nba,
                                predicted = AST_EQUIV),
            REB_accuracy = rmse(actual = TRB_nba,
                                predicted = REB_EQUIV))
