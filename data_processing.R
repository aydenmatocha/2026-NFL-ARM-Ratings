library(tidyverse)
library(nflfastR)
library(nflreadr)

schedule <- load_schedules(2026)
teams <- unique(schedule$home_team)
pbp <- load_pbp(2026)

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