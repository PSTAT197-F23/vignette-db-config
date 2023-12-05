library(DBI)  # Contains functions for interacting with database
library(RSQL)  # Generate and process SQL queries in R
library(RSQLite)  # Can create an in-memory SQL database
library(tidyverse)

## Load each file in 

appearances <- read_csv('data/preprocessed/appearances.csv')
games <- read_csv('data/preprocessed/games.csv')
leagues <- read_csv('data/preprocessed/leagues.csv')
players <- read_csv('data/preprocessed/players.csv')
shots <- read_csv('data/preprocessed/shots.csv')
teams <- read_csv('data/preprocessed/teams.csv')
teamstats <- read_csv('data/preprocessed/teamstats.csv')


## Create the database connection
soccer_con <- dbConnect(drv = RSQLite::SQLite(),
                           dbname = "data/soccer")

# Check that database is empty
dbListTables(soccer_con)


## Load tables into database
dbWriteTable(conn = soccer_con,
             name = "appearances",
             value= appearances)

dbWriteTable(conn = soccer_con,
             name = "games",
             value= games)

dbWriteTable(conn = soccer_con,
             name = "leagues",
             value= leagues)

dbWriteTable(conn = soccer_con,
             name = "players",
             value= players)

dbWriteTable(conn = soccer_con,
             name = "shots",
             value= shots)

dbWriteTable(conn = soccer_con,
             name = "teams",
             value= teams)

dbWriteTable(conn = soccer_con,
             name = "teamstats",
             value= teamstats)


# Check that database has new tables
dbListTables(soccer_con)


## Useful functions
dbListFields(soccer_con, "teams")  # list columns in teams dataframe

sql_teams <- tbl(soccer_con, "teams")  # table from SQL connection
class(sql_teams)

df_teams <- collect(sql_teams)
class(df_teams)


## Create SQL queries
dbGetQuery(soccer_con, "SELECT count(gameID)
                        FROM shots
                        WHERE shotResult = 'OwnGoal'")


## Make sure to close the connection when you're done to conserve memory
DBI::dbDisconnect(soccer_con)


