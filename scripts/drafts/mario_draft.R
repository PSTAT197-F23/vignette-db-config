# Mario Tapia-Pacheco

library(dplyr)
library(naniar)
library(tidyverse)

## Load each file in 

appearances <- read_csv('data/raw/appearances.csv')
games <- read_csv('data/raw/games.csv')
leagues <- read_csv('data/raw/leagues.csv')
players <- read_csv('data/raw/players.csv')
shots <- read_csv('data/raw/shots.csv')
teams <- read_csv('data/raw/teams.csv')
teamstats <- read_csv('data/raw/teamstats.csv')

## Check for missing data in each csv file

# vis_miss(appearances)  # too large for visualization
sum(is.na(appearances))
# no missing data

vis_miss(games)  
# less than 0.1% of columns are missing, we can drop na
games <- drop_na(games)

vis_miss(leagues)  
# no missing data

vis_miss(players)
# no missing data

# vis_miss(shots)  # too large for visualization
sum(is.na(shots))
# 84,344 NA values, let's inspect
colnames(shots)
sum(is.na(shots$gameID))  # no missing values
sum(is.na(shots$shooterID))  # no missing values
sum(is.na(shots$assisterID))  # 84,344 missing values
# it makes sense that all missing values are in the assisterID column since 
# not every goal scored has an assist. No need to impute NA values

vis_miss(teams) 
# no missing data

vis_miss(teamstats) 
# less than 0.1% of columns are missing, we can drop na
teamstats <- drop_na(teamstats)

## write the preprocessed data into corresponding folder

write_csv(appearances, 'data/preprocessed/appearances.csv')
write_csv(games, 'data/preprocessed/games.csv')
write_csv(leagues, 'data/preprocessed/leagues.csv')
write_csv(players, 'data/preprocessed/players.csv')
write_csv(shots, 'data/preprocessed/shots.csv')
write_csv(teams, 'data/preprocessed/teams.csv')
write_csv(teamstats, 'data/preprocessed/teamstats.csv')
