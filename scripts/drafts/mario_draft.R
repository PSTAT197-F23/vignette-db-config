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

# we can use SELECT to select certain variables and 
# FROM to choose a specific table in the database
dbGetQuery(soccer_con, "SELECT shooterID
                        FROM shots") %>%
  head()

# we can use * in the SELECT statement to select ALL variables in a table
dbGetQuery(soccer_con, "SELECT *
                        FROM shots") %>%
  head()

# we can use functions on variables in the SELECT statement
dbGetQuery(soccer_con, "SELECT count(gameID)
                        FROM shots") 

# we can use a WHERE statement to get obs that match a specific condition
dbGetQuery(soccer_con, "SELECT count(gameID)
                        FROM shots
                        WHERE shotResult = 'OwnGoal'")

# we can use an ORDER BY statement to order observations
dbGetQuery(soccer_con, "SELECT gameID, shotType
                        FROM shots
                        WHERE shotResult = 'OwnGoal'
                        ORDER BY shooterID desc") %>%
  head()

# we can use a HAVING statement, similar to WHERE however it needs to be used 
# with a GROUPBY statment
dbGetQuery(soccer_con, "SELECT gameID
                        FROM shots
                        WHERE shotResult = 'OwnGoal'
                        GROUP BY gameID
                        HAVING shotType = 'LeftFoot'") %>%
  head()


## Joining Tables

# there are several kinds of joins that allow us to join tables and extract 
# information that is stored separately
# * insert SQL joins picture *

# in order to join tables, we need what's called a primary key
# this key will allow us to relate the tables and join them
# primary key can uniquely identify an observation
# foreign key establishes a relationship with another table's primary key 

# we can use a left join 
# find the num of goals a team scored when they scored an own goal in the game
dbGetQuery(soccer_con, "SELECT count(homeGoals)
                        FROM games
                        LEFT JOIN shots
                        ON games.gameID = shots.gameID
                        WHERE shotResult = 'OwnGoal'") %>%
  head()

# an inner join
dbGetQuery(soccer_con, "SELECT count(homeGoals)
                        FROM games
                        INNER JOIN shots
                        ON games.gameID = shots.gameID
                        WHERE shotResult = 'OwnGoal'") %>%
  head()

# inspect the data and try joining multiple tables
dbGetQuery(soccer_con, "SELECT DISTINCT name 
                        FROM players
                        JOIN shots
                        ON players.playerID = shots.shooterID
                        JOIN games
                        ON shots.gameID = games.gameID
                        WHERE season = '2015'") %>%
  head()
# players who took shots in 2015

## Make sure to close the connection when you're done to conserve memory
DBI::dbDisconnect(soccer_con)
