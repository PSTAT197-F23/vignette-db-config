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
                        dbname = "data/databases/soccer")

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

## Let's go through some more example queries and introduce a few more essential 
# concepts


# CONCEPT 1: CREATING COLUMNS
#   Creating and modifying columns enables us to derive new data from existing datasets, 
#   enhancing our ability to analyze and understand our data.


# Example 1.1) This query adds together the yellowCard and redCard columns to create a new column totalCards. 
# Thus indicating the total number of cards a player received in a game.
dbGetQuery(soccer_con, "SELECT playerID, gameID, (yellowCard + redCard) AS totalCards
                        FROM appearances
                        LIMIT 50")

# Example 1.2) This query adds the homeGoals and awayGoals columns to create a new column totalGoals.
# Thus indicating the total number of goals scored in each game.
dbGetQuery(soccer_con, "SELECT gameID, homeGoals, awayGoals, homeGoals + awayGoals AS totalGoals
                        FROM games")

# Example 1.3) This query creates a new column uniqueID by concatenating playerID and gameID with a hyphen in between.
dbGetQuery(soccer_con, "SELECT playerID, gameID, playerID || '-' || gameID AS uniqueID
                        FROM appearances")


# CONCEPT 2: AGGREGATION FUNCTIONS
#   Aggregation functions enable us to find averages, maximums, minimums, counts, sums, etc. 


# Example 2.1) This query calculates the AVERAGE number of goals scored by each team in each season.
dbGetQuery(soccer_con, "SELECT teamID, season, AVG(goals) AS average_goals
                        FROM teamstats
                        GROUP BY teamID, season
                        LIMIT 50")

# Example 2.2) This query finds the MAXIMUM and MINIMUM number of fouls committed in a single game across all records.
dbGetQuery(soccer_con, "SELECT MAX(fouls) AS max_fouls, MIN(fouls) AS min_fouls
                        FROM teamstats")

# Example 2.3) This query COUNTS the number of wins ('W'), losses ('L'), and draws ('D') for each team.
dbGetQuery(soccer_con, "SELECT teamID, result, COUNT(*) AS game_count
                        FROM teamstats
                        GROUP BY teamID, result
                        LIMIT 50")

# Example 2.4) This query SUMS up the total number of shots and shots on target for home ('h') and away ('a') games separately.
dbGetQuery(soccer_con, "SELECT location, SUM(shots) AS total_shots, SUM(shotsOnTarget) AS shots_on_target
                        FROM teamstats
                        GROUP BY location")


# CONCEPT 3: SUBQUERIES
#   Subqueries are a powerful feature that allow us to use the result of one query as a condition or reference in another.
#   Thus enabling complex and layered data analysis within a single statement.


# Example 3.1)
# This query finds the percentage of each type of shotResult within the Shots table.
# In this example, the subquery is used to calculate the total number of shots in the dataset
# This is a necessary step to determine the percentage of each shot result
dbGetQuery(soccer_con, "SELECT shotResult, 
                               COUNT(*) * 100.0 / (SELECT COUNT(*) FROM shots) AS percentage
                        FROM shots
                        GROUP BY shotResult")

# Example 3.2)
# In this query, the subquery shooterAverages calculates the average xGoal for each shooterID.
# The main query then selects those shooters whose average xGoal is higher than the overall average xGoal calculated across all shots. 
# Ultimately, we find the shooters that have a higher average xGoal than the average xGoal across the table.
dbGetQuery(soccer_con, "SELECT shooterID AS bestShooters
                        FROM (
                              SELECT shooterID, AVG(xGoal) AS avgShooterXGoal
                              FROM shots
                              GROUP BY shooterID) AS shooterAverages
                        WHERE avgShooterXGoal > (
                              SELECT AVG(xGoal) FROM shots)
                        LIMIT 50")

## Make sure to close the connection when you're done to conserve memory
DBI::dbDisconnect(soccer_con)