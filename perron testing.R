library(tidyverse)
source("data_processing.R")

# Add average opp rating to final_ratings
# get max value and min value
run_perron <- function(input_df) {
  teams2 <- input_df$team
  average_ratings <- c()
  
  for (i in teams2) {
    team_schedule <- get_team_standings(i)
    opponents <- unique(team_schedule$opponent)
    sum_ratings <- 0
    for (j in opponents) {
      opp_rating <- input_df %>% filter(team == j) %>% pull(unique(total))
      sum_ratings <- sum_ratings + opp_rating
    }
    average_rating <- round(sum_ratings / length(opponents), 2)
    average_ratings <- c(average_ratings, average_rating)
  }
  average_ratings
  
  max_avg_rtg <- max(average_ratings)
  min_avg_rtg <- min(average_ratings)
  avg_rtg_range <- max_avg_rtg - min_avg_rtg
  
  input_df["avg_rtg"] = average_ratings
  
  input_df$sos <- round((input_df$avg_rtg - min_avg_rtg) / (avg_rtg_range / 2), 2)
  
  input_df$total <- input_df$win_pct + input_df$point_diff + input_df$off_epa + input_df$def_epa + input_df$sos
  
  input_df <- input_df %>% arrange(-total) %>% select(-avg_rtg)
  
  return(input_df)
}

perron1 <- run_perron(final_ratings)
perron2 <- run_perron(perron1)
perron3 <- run_perron(perron2)
