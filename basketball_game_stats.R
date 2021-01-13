library(robotstxt)
library(rvest)
library(stringr)
library(dplyr)
library(RSelenium)
library(XML)
library(RPostgreSQL)
library(httr)
library(tidyr)

drv <- PostgreSQL()
# Chris Cross
pw <- readline(prompt="Enter pw: ")
conn <- dbConnect(drv,dbname = 'sport_sett_development', 
                 host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'postgres',
                 password = pw)

isPostgresqlIdCurrent(conn)

# Grab the game stats
game_stats <- dbGetQuery(conn,'
 select * from statistics_team_in_games stig
  join team_in_games tig on stig.team_in_game_id=tig.id 
  join "statistics" s2 on stig.statistic_id=s2.id
  join statistic_types st on s2.statistic_type_id=st.id
  join team_in_seasons_team_names tistn on tistn.team_in_season_id=tig.team_in_season_id /* Next two grab team name*/
  join team_names tn  on tn.id=tistn.team_name_id 
  join games on games.id=tig.game_id /* Grab Date*/
')

# dplyr cant get same name columns
gs <- dplyr::select(game_stats, c('game_id', 'winner', 'home', 'points',
                                  'quantity', 'code', 'name',  'date',
                                  'attendance', 'code', 'name'))
##### WARNING: Looks like win loss isn't correct. Fixing via boxscores,
##### need to examine why and if the other statistics are still accurate

# Select the relevant stats and long -> wide
gs <- game_stats[,c(5:9,14,19,20,22,29,37:39)] %>%
  arrange(game_id, home)
colnames(gs) <- c('game_id', 'winner',  'home', 'points', 'game_number', 'quantity', 'stat_code',
                  'counts_for_double', 'multiplier', 'team_code', 'date', 'attendance', 'stage')

gs_wide <- spread(gs, stat_code, quantity)





