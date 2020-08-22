library(yardstick)
library(tidyverse)

combo <- read_csv("data/combo.csv")
g_league <- read_csv("data/g_league.csv")
nba <- read_csv("data/nba.csv")


#### Basic Equivalency ####

# Calculating general ratios for stats
# Shows how much a G-League stat is worth in the NBA
ratios_by_player <- combo %>%
  mutate(PTS_RATIO = PTS_nba / PTS_gleague,
         AST_RATIO = AST_nba / AST_gleague,
         REB_RATIO = TRB_nba / TRB_gleague) %>%
  select(Season, Player:Tm_nba, Tm_gleague, PTS_RATIO:REB_RATIO)
ratios <- ratios_by_player %>%
  summarize_at(.vars = vars(PTS_RATIO:REB_RATIO),
               .funs = list(mean))

# Finding general NBA equivalent stats for all G-League players
g_league_equiv <- g_league %>%
  mutate(PTS_EQUIV = PTS * ratios$PTS_RATIO,
         AST_EQUIV = AST * ratios$AST_RATIO,
         REB_EQUIV = TRB * ratios$REB_RATIO) %>%
  select(Season, Player, PTS, PTS_EQUIV, AST, AST_EQUIV,
         TRB, REB_EQUIV)

# Measuring accuracy of this generalized method
map2_df(.x = combo %>%
          inner_join(g_league_equiv, by = c("Player", "Season")) %>%
          select(PTS_nba, AST_nba, TRB_nba),
        .y = combo %>%
          inner_join(g_league_equiv, by = c("Player", "Season")) %>%
          select(PTS_EQUIV, AST_EQUIV, REB_EQUIV),
        .f = ~ rmse_vec(truth = .x, estimate = .y))


#### Weighted Equivalency ####

# Want to weight equivalency by how similar G-league players are

# We'll specify columns used to get similarity scores
similarity_columns <- c("PTS", "AST", "TRB", "Age", "FG%", "3P%", "FT%",
                        "STL", "BLK", "TOV", "PF", "3PA", "FTA", "FGA",
                        "Age")
# And columns that rep player seasons from our combo tibble
# We only care about how similar players are to these players
combo_columns <- (combo %>%
  mutate(player_season = paste(Player, Season, sep = " ")))$player_season

# Creating a similarity tibble
g_league_sim <- g_league %>%
  mutate(player_season = paste(Player, Season, sep = " ")) %>%
  select(player_season, similarity_columns) %>%
  # Scale all similarity columns
  mutate_at(.vars = all_of(similarity_columns),
            .funs = ~ scale(.)) %>%
  column_to_rownames(var = "player_season") %>%
  # Creating distance matrix
  dist() %>%
  as.matrix() %>%
  as.data.frame() %>%
  # Selecting only columns of players who have played in both leagues
  select(all_of(combo_columns)) %>%
  rownames_to_column(var = "player_season")

# Changing the similarity tibble from wide to long
# Normalizing similarity scores between 0 and 1
g_league_sim_long <- g_league_sim %>%
  pivot_longer(cols = ends_with(as.character(2015:2020)),
               names_to = "player_compare") %>%
  mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
  mutate(value = 1 - value)

# Creating a function that will calculate the weighted average ratio
weighted_ratio <- function(player, season){
  input_player_season <- paste(player, season)
  
  ratio_df <- g_league_sim_long %>%
    filter(player_season == input_player_season) %>%
    inner_join(ratios_by_player %>%
                 mutate(player_season = paste(Player, Season, sep = " ")),
               by = c("player_compare" = "player_season"))
  
  summarized_ratios <- ratio_df %>%
    group_by(player_season) %>%
    summarize_at(.vars = vars(PTS_RATIO, AST_RATIO, REB_RATIO),
                 .funs = ~ weighted.mean(., w = value))
  
  weighted_equiv <- g_league %>%
    mutate(player_season = paste(Player, Season, sep = " ")) %>%
    filter(player_season == input_player_season) %>%
    mutate(PTS_EQUIV = PTS * summarized_ratios$PTS_RATIO,
           AST_EQUIV = AST * summarized_ratios$AST_RATIO,
           REB_EQUIV = TRB * summarized_ratios$REB_RATIO) %>%
    select(Season, Player, PTS, PTS_EQUIV, AST, AST_EQUIV,
           TRB, REB_EQUIV)
  
  most_similar <- ratio_df %>%
    slice_max(value, n = 10) %>%
    select(value:Age_nba, PTS_RATIO:REB_RATIO)
  
  output_list <- list(weighted_equiv = weighted_equiv,
                      most_similar = most_similar)
  
  output_list
}

weighted_ratio(player = "Jordan Crawford", season = 2017)

# Looping through G-leaguers and applying weighted_ratio to each
weighted_equiv <- map2_df(.x = g_league$Player,
                          .y = g_league$Season,
                          .f = ~ weighted_ratio(.x, .y)[[1]])

map2_df(.x = combo %>%
          select(PTS_nba, AST_nba, TRB_nba),
        .y = weighted_equiv %>%
          inner_join(combo %>%
                       select(Player, Season),
                     by = c("Player", "Season")) %>%
          select(PTS_EQUIV, AST_EQUIV, REB_EQUIV),
        .f = ~ rmse_vec(truth = .x, estimate = .y))
