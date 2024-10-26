## code to prepare `data_cleaning_script` dataset goes here

library(tidyverse)
library(rlang)
library(showtext)
library(dplyr)
library(tidyr)
library(knitr)

soccer <- read_csv("data-raw/soccer21-22.csv")

full_season_stats <- function(x, teamName, resultColumn = FTR, resultPrefix = FT) {

  result <- enquo(resultColumn)
  home_goals <- new_quosure(sym(paste0(quo_name(enquo(resultPrefix)), 'HG')))
  away_goals <- new_quosure(sym(paste0(quo_name(enquo(resultPrefix)), 'AG')))

  home_game_summary <- x %>%
    filter(HomeTeam == teamName) %>%
    summarise(MP = n(),
              W = sum(!!result == 'H'),
              D = sum(!!result == 'D'),
              L = sum(!!result == 'A'),
              GF = sum(!!home_goals),
              GA = sum(!! away_goals),
              GD = GF - GA,
              Points = W * 3 + D)

  away_game_summary <- x %>%
    filter(AwayTeam == teamName) %>%
    summarise(MP = n(),
              W = sum(!!result == 'A'),
              D = sum(!!result == 'D'),
              L = sum(!!result == 'H'),
              GF = sum(!!away_goals),
              GA = sum(!!home_goals),
              GD = GF - GA,
              Points = W * 3 + D)

  final_stats <- (home_game_summary %>% rownames_to_column()) %>%
    bind_rows(away_game_summary %>% rownames_to_column()) %>%
    group_by(rowname) %>%
    summarise_all(sum) %>%
    select(-rowname) %>%
    mutate(Team = teamName) %>%
    select(Team, everything())

  return(final_stats)
}


full_league_table <- tibble()

for(team in unique(soccer$HomeTeam)){
  full_league_table <- rbind(full_league_table, full_season_stats(soccer, team, FTR, FT))
}

full_league_table <- full_league_table %>%
  arrange(desc(Points), desc(GD)) %>%
  rownames_to_column() %>%
  rename('Pos.' = rowname)

full_league_table <- full_league_table %>% mutate(finish_time = 'full_time')
half_time_results <- tibble()

for(team in unique(soccer$HomeTeam)){
  half_time_results <- rbind(half_time_results, full_season_stats(soccer, team, HTR, HT))
}

half_time_results <- half_time_results %>%
  arrange(desc(Points), desc(GD)) %>%
  rownames_to_column() %>%
  rename('Pos.' = rowname)

half_time_results <- half_time_results %>% mutate(finish_time = 'half_time')

usethis::use_data(full_league_table, overwrite = TRUE)
usethis::use_data(half_time_results, overwrite = TRUE)
