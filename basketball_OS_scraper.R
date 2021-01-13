library(robotstxt)
library(rvest)
library(stringr)
library(dplyr)
library(RSelenium)
library(XML)
library(RPostgreSQL)
library(httr)



remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)
remDr$open()

paths_allowed('https://www.oddsshark.com/nba/database')

cities_list <-c('Atlanta', 'Charlotte', 'Chicago', 'Cleveland',
                'Dallas', 'Golden State', 'Indiana', 'LA Clippers',
                'LA Lakers', 'Miami', 'Milwaukee', 'Minnesota',
                'New Orleans', 'Oklahoma City', 'Orlando', 'Philadelphia',
                'Portland', 'Sacramento', 'San Antonio', 'Washington',
                'New York', 'Toronto', 'Memphis', 'Phoenix', 
                'Boston', 'Brooklyn', 'Houston', 'Detroit',
                'Utah', 'Denver')

cities_list <- sort(cities_list)

ScrapeHistoricalOddSharkDB <- function(cities_list, sport='nba'){
  
  os_dbURL <- paste('https://www.oddsshark.com/', sport, '/database', sep ='')
  remDr$close()
  remDr$open()
  
  remDr$navigate(os_dbURL)
  remDr$maxWindowSize()
  first <- T
  count<-0
  for (i in cities_list[-1]){
    print(paste('Working on', i))
    other_cities <- cities_list[-count]
    count <- count+1
    
    remDr$navigate(os_dbURL)
    remDr$maxWindowSize()
    
    for (j in other_cities){
      
      print(paste(i, 'vs', j))
      remDr$navigate(os_dbURL)
      remDr$maxWindowSize()
      
      # Select all the elements for db query
      team_search <- remDr$findElement(using = 'css selector', '#team-search-h2h')
      opp_search <- remDr$findElement(using = 'css selector', '#opponent-search-h2h')
      num_games30 <- remDr$findElement(using = 'xpath', '/html/body/div[3]/div/div[2]/div[2]/div[3]/div[1]/div/div/div/div/div[2]/form/table/tbody/tr[3]/td/div/div[3]')
      game_type <- remDr$findElement(using = 'css selector', '#chalk-select-game-type-h2h')
      home_awayANY <- remDr$findElement(using = 'xpath', '/html/body/div[3]/div/div[2]/div[2]/div[3]/div[1]/div/div/div/div/div[2]/form/table/tbody/tr[6]/td/div/div[1]')
      select_oddsUNDER <- remDr$findElement(using = 'css selector', '#chalk-select-odds-h2h')
      min_odds <- remDr$findElement(using = 'css selector', '#txt-min-h2h')
      max_odds <- remDr$findElement(using = 'css selector', '#txt-max-h2h')
      search_button <-remDr$findElement(using = 'css selector', '#submit-h2h')
      
      # Send the requested values
      team_search$sendKeysToElement(list(i))
      opp_search$sendKeysToElement(list(j))
      num_games30$clickElement()
      
      game_type$sendKeysToElement(list('REG'))
      
      game_typeREG <- remDr$findElement(using = 'xpath', '/html/body/div[3]/div/div[2]/div[2]/div[3]/div[1]/div/div/div/div/div[2]/form/table/tbody/tr[7]/td/div/div/select/option[2]')
      game_typeREG$clickElement()
      
      home_awayANY$clickElement()
      
      select_oddsUNDER <- remDr$findElement(using = 'xpath', '/html/body/div[3]/div/div[2]/div[2]/div[3]/div[1]/div/div/div/div/div[2]/form/table/tbody/tr[7]/td/div/div/select/option[5]')
      select_oddsUNDER$clickElement()
      
      underdog <- remDr$findElement(using = 'css selector', '#chalk-select-odds-h2h > option:nth-child(5)')
      underdog$clickElement()
      
      min_odds$sendKeysToElement(list('1'))
      max_odds$sendKeysToElement(list('60'))
      
      search_button$clickElement()
      
      # Get the table
      Sys.sleep(5)
      
      os_page1 <- read_html(remDr$getPageSource()[[1]])
      #/html/body/div[3]/div/div[2]/div[2]/div[3]/div/div/div/div
      potential_table <- os_page1 %>%
        html_node(xpath = '/html/body/div[3]/div/div[2]/div[2]/div[3]/div/div/div/div/table[2]')
      if (length(potential_table) != 0){
        temp_table <- potential_table %>%
          html_table()
        #print(paste('Actual Table Names:', colnames(temp_table)))
      }
      # Clean this up will ya? All it does is add an NA entry when there have been no games in the OS db for the 
      # teams at the current params
      else{
        x1<- c('Date', 'Away', 'Score', 'Home', 'Score',  'Result', 'Home Spread', 'ATS', 'Total', 'OU')
        x2<- rep(0, length(x1))
        x <- as.matrix(x1,x2)
        x <- t(x)
        x <- data.frame(x)
        colnames(x) <- x1
        x[1,] <- NA
        temp_table <- x
        #print(paste('NA Table Names:', colnames(temp_table)))
      }
      # Legacy table finding attempts
      #html_node(xpath = '//*[(@id = "block-system-main")]//td') %>%
      #html_node('.table-wrapper__overflow')
      #html_table()
      
      if (first==T){
        final_table <- temp_table
        first <- F
      }
      else{
        final_table <- rbind(final_table, temp_table)
      }
    }      
  } 
  return(final_table)
}

historical_os <- ScrapeHistoricalOddSharkDB(cities_list, sport='nba')

# Ran overnight 12/19/20
write.csv(historical_os, '~/Fall2020/bballHisOSCL_raw2.csv')
 