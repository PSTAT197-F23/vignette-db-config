library(DBI)  # Contains functions for interacting with database
library(RSQL)  # Generate and process SQL queries in R
library(RSQLite)  # Can create an in-memory SQL database
library(tidyverse)
library(tidymodels)
library(themis)
library(ggplot2)
library(corrplot)
library(kknn)
library(ranger)
library(randomForest)
library(xgboost)
library(vip)

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


## Modeling

# We can practice modeling and using queries 
# create three models to predict whether or not a team will win a game

# create query to get table with relevant data
mdl_data <- dbGetQuery(soccer_con, "SELECT *
                        FROM games
                        FULL OUTER JOIN teamstats
                        ON games.gameID = teamstats.gameID") %>%
  collect()

mdl_data <- mdl_data %>%
  select(c(36,39:50))

mdl_data$result <- factor(mdl_data$result, levels = c("L", "W", "D"), 
                          labels = c(0,1,2))

# analyze correlation between variables
mdl_data %>% 
  select(where(is.numeric)) %>% 
  cor() %>% 
  corrplot(type = 'lower', diag = FALSE, 
           method = 'color')

# check for class imbalances
mdl_data %>%
  ggplot() +
  geom_bar(aes(x = result, fill = result), color = "black") +
  theme_minimal()

# set seed for reproducibility
set.seed(1208)

# split data into training, testing, and create 5 folds for cv
mdl_split <- initial_split(mdl_data, strata = "result", prop = 0.8)

mdl_train <- training(mdl_split)
mdl_test <- testing(mdl_split)

mdl_folds <- vfold_cv(mdl_train, strata = "result", v = 5)

# create recipe
mdl_recipe <- recipe(
  result ~ goals + shots + shotsOnTarget + deep + corners, 
  data = mdl_train) %>% 
  step_upsample(result) %>%
  step_dummy(all_nominal_predictors()) %>%  # dummy-code all nominal preds
  step_normalize(all_numeric_predictors())  # normalize preds

prep(mdl_recipe) %>% bake(new_data = mdl_train)  # prep and bake recipe

### K-Nearest Neighbors

# K-nearest neighbors is a model that uses information about the observations 
# closest to our new observations to classify them. It can be used for both 
# regression and classification problems, but we will use it for classification. 
# The model gets information about the "k" nearest data points and classifies 
# new data points based on the majority.

# create knn model
knn_model <- nearest_neighbor(neighbors=tune()) %>% # tune n
  set_engine("kknn") %>% 
  set_mode("classification")

# create knn workflow
knn_wflow <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(mdl_recipe)

# create grid to tune neighbors
knn_tune_grid <- grid_regular(neighbors(range = c(10,500)),
                              levels = 5)

# tune neighbors
# tune_knn <- tune_grid(
#   knn_wflow,
#   resamples = mdl_folds,
#   grid = knn_tune_grid
# )

# write results to rds file to reduce knit time
# write_rds(tune_knn, file = 'data/tuning/knn_tune.rds')

# read outputted rds file
tune_knn <- read_rds('data/tuning/knn_tune.rds')

# visualize model performance by number of neighbors
autoplot(tune_knn)

# manually inspect model performance by number of neighbors
collect_metrics(tune_knn)

# select best performing model based on accuracy
best_knn <- select_best(tune_knn,
                        metric = "accuracy",
                        neighbors
)

# finalize workflow
final_knn_wf <- finalize_workflow(knn_wflow,
                                  best_knn)

# fit model to training data
final_knn <- fit(final_knn_wf, 
                    data = mdl_train)

# evaluate performance
knn_auc <- augment(final_knn, new_data = mdl_train) %>%
  roc_auc(result, .pred_0:.pred_2)
knn_acc <- augment(final_knn, new_data = mdl_train) %>%
  accuracy(result, .pred_class)
knn_train_results <- bind_rows(knn_auc, knn_acc) %>%
  tibble() %>% mutate(metric = c("roc auc", "acc")) %>%
  select(metric, .estimate)

# display
knn_train_results

### Extreme Gradient Boosting

# Explain

# create xg boost model
xg_model <- boost_tree(mtry = tune(), 
                            trees = tune(), 
                            learn_rate = tune()) %>%
  set_engine("xgboost") %>% 
  set_mode("classification")

# create xg boost workflow
xg_wflow <- workflow() %>% 
  add_model(xg_model) %>% 
  add_recipe(mdl_recipe)

# create grid to tune hyper parameters
xg_tune_grid <- grid_regular(mtry(range = c(1, 6)), 
                        trees(range = c(200, 600)),
                        learn_rate(range = c(-10, -1)),
                        levels = 5)

# tune hyper parameters
# tune_xg <- tune_grid(
#   xg_wflow,
#   resamples = mdl_folds,
#   grid = xg_tune_grid
# )

# write results to rds file to reduce knit time
# write_rds(tune_xg, file = 'data/tuning/xg_tune.rds')

# read outputted rds file
tune_xg <- read_rds('data/tuning/xg_tune.rds')

# visualize model performance by number of neighbors
autoplot(tune_xg)

# manually inspect model performance by number of neighbors
collect_metrics(tune_xg)

# select best performing model based on accuracy
best_xg <- select_best(tune_xg,
                       metric = "roc_auc",
                       mtry,
                       trees,
                       learn_rate
)

# finalize workflow
final_xg_wf <- finalize_workflow(xg_wflow,
                                  best_xg)

# fit model to training data
final_xg <- fit(final_xg_wf, 
                 data = mdl_train)

# evaluate performance
xg_auc <- augment(final_xg, new_data = mdl_train) %>%
  roc_auc(result, .pred_0:.pred_2)
xg_acc <- augment(final_xg, new_data = mdl_train) %>%
  accuracy(result, .pred_class)
xg_train_results <- bind_rows(xg_auc, xg_acc) %>%
  tibble() %>% mutate(metric = c("roc auc", "acc")) %>%
  select(metric, .estimate)

# display
xg_train_results


### Choosing Best Model Based on Training Data

# add model names to results
knn_train_results <- knn_train_results %>%
  mutate(model = "K-Nearest Neighbors")
xg_train_results <- xg_train_results %>%
  mutate(model = "Extreme Gradient Boosting")

# combine into one table to easily compare
combined_train_results <- rbind(knn_train_results,
                                xg_train_results)

# display
combined_train_results

##### IF WE NEED TO DELETE TABLES

# dbRemoveTable(soccer_con, "appearances")
# dbRemoveTable(soccer_con, "games")
# dbRemoveTable(soccer_con, "leagues")
# dbRemoveTable(soccer_con, "players")
# dbRemoveTable(soccer_con, "shots")
# dbRemoveTable(soccer_con, "teams")
# dbRemoveTable(soccer_con, "teamstats")

## Make sure to close the connection when you're done to conserve memory
DBI::dbDisconnect(soccer_con)
