library(tidyverse)
library(nflfastR)
library(nflreadr)

schedule <- load_schedules(2026)
teams <- unique(schedule$home_team)
pbp <- load_pbp(2026)
win_percentages <- c()
point_diffs <- c()
off_epas <- c()
def_epas <- c()
sos <- c()

for (i in teams) { # Get win%, point diff, off epa, and def epa
  team_stats <- get_team_stats(i)
  win_percentages <- c(win_percentages, team_stats[1])
  point_diffs <- c(point_diffs, team_stats[2])
  
  team_epas <- get_team_epa(i)
  off_epas <- c(off_epas, team_epas[1])
  def_epas <- c(def_epas, team_epas[2])
}

initial_values <- data.frame( # Create df with the initial values
  team = teams,
  win_pct = win_percentages,
  point_diff = point_diffs,
  off_epa = off_epas,
  def_epa = def_epas
)

# Calculate initial ratings
min_pd <- min(initial_values$point_diff)
max_pd <- max(initial_values$point_diff)
pd_range <- max_pd - min_pd

initial_ratings <- get_init_ratings(initial_values)

# Calculate SOS
for (i in teams) {
  team_schedule <- get_team_standings(i)
  opponents <- unique(team_schedule$opponent)
  win_percentages_removed <- c()
  point_diffs_removed <- c()
  off_epas_removed <- c()
  def_epas_removed <- c()
  
  for (j in opponents) { # Calculate average initial rating of opponents (not including stats against current team)
    team_stats <- get_team_stats_removed(j, i)
    win_percentages_removed <- c(win_percentages_removed, team_stats[1])
    point_diffs_removed <- c(point_diffs_removed, team_stats[2])
    
    team_epas <- get_team_epa_removed(j, i)
    off_epas_removed <- c(off_epas_removed, team_epas[1])
    def_epas_removed <- c(def_epas_removed, team_epas[2])
  }
  
  initial_values_removed <- data.frame( # Create df with the initial values
    team = teams,
    win_pct = win_percentages_removed,
    point_diff = point_diffs_removed,
    off_epa = off_epas_removed,
    def_epa = def_epas_removed
  )
  
  initial_ratings_removed <- get_init_ratings(initial_values_removed)
  sos <- c(sos, mean(initial_ratings_removed$total))
}

min_sos <- min(sos)
max_sos <- max(sos)
sos_range <- max_sos - min_sos

sos_ratings <- get_sos_rating(sos)

final_ratings <- initial_ratings %>% select(-total)
final_ratings$sos <- sos_ratings
final_ratings <- final_ratings %>%
  mutate(total = win_pct + point_diff + off_epa + def_epa + sos) %>%
  arrange(-total)

# Helper functions

get_team_standings <- function(team_input) {
  team_schedule <- schedule %>% filter(home_team == team_input | away_team == team_input, !is.na(home_score)) %>%
    select(home_team, home_score, away_team, away_score) %>%
    mutate(opponent = ifelse(home_team == team_input, away_team, home_team),
           points = ifelse(home_team == team_input, home_score, away_score),
           opp_points = ifelse(home_team == team_input, away_score, home_score),
           win = ifelse(points > opp_points, 1, 0)) %>%
    select(opponent, points, opp_points, win)
  
  return(team_schedule)
}

get_team_standings_removed <- function(team_input, team_removed) {
  team_schedule <- get_team_standings(team_input) %>%
    filter(opponent != team_removed)
  
  return(team_schedule)
}

get_team_stats <- function(team_input) {
  team_schedule <- get_team_standings(team_input)
  win_percentage <- mean(team_schedule$win)
  point_diff <- sum(team_schedule$points) - sum(team_schedule$opp_points)
  team_stats <- c(win_percentage, point_diff)
  
  return(team_stats)
}

get_team_stats_removed <- function(team_input, team_removed) {
  team_schedule <- get_team_standings_removed(team_input, team_removed)
  win_percentage <- mean(team_schedule$win)
  point_diff <- sum(team_schedule$points) - sum(team_schedule$opp_points)
  team_stats <- c(win_percentage, point_diff)
  
  return(team_stats)
}

get_team_epa <- function(team_input) {
  off_epa <- pbp %>% filter(posteam == team_input, !is.na(epa)) %>% 
    summarize(mean_epa = mean(epa)) %>% 
    pull(mean_epa)
  
  def_epa <- pbp %>% filter(defteam == team_input, !is.na(epa)) %>% 
    summarize(mean_epa = mean(epa)) %>% 
    pull(mean_epa)
  
  team_epas <- c(round(off_epa, 3), round(def_epa, 3))
  
  return(team_epas)
}

get_team_epa_removed <- function(team_input, team_removed) {
  off_epa <- pbp %>% filter(posteam == team_input, !is.na(epa), defteam != team_removed) %>% 
    summarize(mean_epa = mean(epa)) %>% 
    pull(mean_epa)
  
  def_epa <- pbp %>% filter(defteam == team_input, !is.na(epa), posteam != team_removed) %>% 
    summarize(mean_epa = mean(epa)) %>% 
    pull(mean_epa)
  
  team_epas <- c(round(off_epa, 3), round(def_epa, 3))
  
  return(team_epas)
}

get_init_ratings <- function(input_df) {
  output <- input_df %>% mutate(
    win_pct = round(win_pct * 2, 2), # min: 0%, mean: 50%, max: 100%
    point_diff = round(((point_diff-min_pd) / pd_range) * 2, 2), # min: min_diff, mean: ?, max: max_diff
    off_epa = round(pmax(pmin((off_epa + .15) * 6.67, 2), 0), 2), # min: -.15, mean: 0, max: .15
    def_epa = round(pmax(pmin((def_epa - .15) * -6.67, 2), 0), 2), # min: .15, mean: 0, max: -.15
    total = win_pct + point_diff + off_epa + def_epa
  ) %>%
    arrange(-total)
  return(output)
}

get_sos_rating <- function(input_vector) {
  input_vector <- round((((input_vector - min_sos) / sos_range) * 2), 2)
  
  return(input_vector)
}
