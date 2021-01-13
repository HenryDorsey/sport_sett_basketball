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
conn <- dbConnect(drv,dbname = 'sport_sett_development', 
                  host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                  port = 5432, # or any other port specified by your DBA
                  user = 'postgres',
                  password = 'xrissxross21')

isPostgresqlIdCurrent(conn)


os_url <- 'https://www.oddsshark.com/nba/odds'


remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

remDr$open()

remDr$navigate(os_url)
remDr$maxWindowSize()
Sys.sleep(5)
os_page <- read_html(remDr$getPageSource()[[1]])

spreads <-  os_page %>%
  html_node(xpath = '//*[@id="op-horizontial-scroll-wrapper"]') %>%
  html_text()
spreads_ <- str_split(spreads, '[+-]')
spreads_ <- str_split( gsub("([+-])","~\\1",spreads), "~" )
spreads <- spreads_[[1]][2:length(spreads_[[1]])]

# Gets names of books except for Skybook which is perpetually empty
first <- T
for (i in c(1:7, 9:10)){
  if (first){
    books <- os_page %>%
      html_node(paste('div.op-book-header:nth-child(', as.character(i), ') > span:nth-child(1) > img:nth-child(1)', sep = '')) %>%
      html_attr('alt')
    first <- F
  }
  else{
    book <- os_page %>%
      html_node(paste('div.op-book-header:nth-child(', as.character(i), ') > a:nth-child(1) > img:nth-child(1)', sep = '')) %>%
      html_attr('alt')
    books <- c(books, book)
  }
}

games <- os_page %>%
  html_node(css = '.op-team-data-wrapper') %>%
  html_text()

# Splits the string along game days
splits <- str_split(games, '[0-9]+')
games_ <- splits[[1]][grep(" Matchup ", splits[[1]])]
# Gets rid of tomorrows games (incomplete book data)
games <- games_[1:which(!grepl("History$", games_), T)]


# function to split into home and away teams
game_filter <- function(game_str){
  teams <- str_sub(game_str, 1, unlist(str_locate(game_str, ' Match')[1])-1)
  temp <- sapply(cities_list, str_extract, string = teams)
  temp <- temp[!is.na(temp)]
  team1 <- str_locate(teams, temp[1])
  team2 <- str_locate(teams, temp[2])
  #print(team1)
  #print(team1[1])
  #print(team2)
  # not sure about this if clause. Meant to determine home vs away
  if (team1[1] == 1){
    away_team <- str_sub(teams, team1[1], team1[2])
    home_team <- str_sub(teams, team2[1], team2[2])
  }
  else{
    away_team <- str_sub(teams, team2[1], team2[2])
    home_team <- str_sub(teams, team1[1], team1[2])
  }
  return(c(away_team, home_team))
}

x <- 'New YorkCharlotte Matchup Line History'
game_filter(x)


first <- T
for (i in 1:length(games)){
  iter <- (i-1)*36
  away_team <- game_filter(games[i])[1]
  home_team <- game_filter(games[i])[2]
  game_str <- str_sub(games[i], 1, unlist(str_locate(games[i], ' Match')[1])-1)
  print(paste(game_str, 'game_str'))
  print(paste(iter, 'iter'))
  print(paste(home_team, 'home_team'))
  print(paste(away_team, 'away_team'))
  if (first==T){
    df <- data.frame(Team=c(away_team, home_team), Home=c(0,1), game=game_str,
                     Opening_ATS=spreads[c(iter+1, iter+3)], Opening_odds=spreads[c(iter+2, iter+4)],
                     BOVADA.LV_ATS=spreads[c(iter+5, iter+7)],BOVADA.LV_odds=spreads[c(iter+6, iter+8)],
                     BetOnline_ATS=spreads[c(iter+9, iter+11)],BetOnline_odds=spreads[c(iter+10, iter+12)],
                     Intertops_ATS=spreads[c(iter+13, iter+15)],Intertops_odds=spreads[c(iter+14, iter+16)],
                     SportsBetting_ATS=spreads[c(iter+17, iter+19)],SportsBetting_odds=spreads[c(iter+18, iter+20)],
                     BetNow_ATS=spreads[c(iter+21, iter+23)],BetNow_odds=spreads[c(iter+22, iter+24)],
                     GTBets_ATS=spreads[c(iter+25, iter+27)],GTBets_odds=spreads[c(iter+26, iter+28)], 
                     FiveDimes_ATS=spreads[c(iter+29, iter+31)],FiveDrimes_odds=spreads[c(iter+30, iter+32)],
                     SportBet_ATS=spreads[c(iter+33, iter+35)],SportBet_odds=spreads[c(iter+34, iter+36)])
    first <- F
  }
  else{
    temp <- data.frame(Team=c(away_team, home_team), Home=c(0,1), game=game_str,
                       Opening_ATS=spreads[c(iter+1, iter+3)], Opening_odds=spreads[c(iter+2, iter+4)],
                       BOVADA.LV_ATS=spreads[c(iter+5, iter+7)],BOVADA.LV_odds=spreads[c(iter+6, iter+8)],
                       BetOnline_ATS=spreads[c(iter+9, iter+11)],BetOnline_odds=spreads[c(iter+10, iter+12)],
                       Intertops_ATS=spreads[c(iter+13, iter+15)],Intertops_odds=spreads[c(iter+14, iter+16)],
                       SportsBetting_ATS=spreads[c(iter+17, iter+19)],SportsBetting_odds=spreads[c(iter+18, iter+20)],
                       BetNow_ATS=spreads[c(iter+21, iter+23)],BetNow_odds=spreads[c(iter+22, iter+24)],
                       GTBets_ATS=spreads[c(iter+25, iter+27)],GTBets_odds=spreads[c(iter+26, iter+28)], 
                       FiveDimes_ATS=spreads[c(iter+29, iter+31)],FiveDrimes_odds=spreads[c(iter+30, iter+32)],
                       SportBet_ATS=spreads[c(iter+33, iter+35)],SportBet_odds=spreads[c(iter+34, iter+36)])
    df <- rbind(df, temp)
  }
}

df[4:21] <- apply(df[4:21], 2, as.numeric)
df[c(1,3)] <- apply(df[c(1,3)], 2, as.character)

# Initial write
#DBI::dbWriteTable(conn, "daily_os_scrape", df)

# Add daily df results
DBI::dbWriteTable(conn, "daily_os_scrape", df, append = TRUE)
