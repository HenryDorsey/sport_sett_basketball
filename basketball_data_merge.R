# Merge OS and historical data
# Get historical_os df from basketball_OS_scraper.R 
# and gs_wide from basketball_game_stats.R

library(hash)
library(zoo)

# First run of historical_os
hos1 <- read.csv2('~/Fall2020/sport_sett_basketball//bballHisOSCL_raw2.csv')
hos1$X <- NULL

# Second run of historical_os
hos2 <- historical_os


'%notin%' <- Negate('%in%')

colnames(historical_os) <- c('Date', 'Away', 'Away_Score', 'Home', 'Home_Score',
                             'Result', 'Home_Spread',  'ATS', 'Total', 'OU')

# Remove improperly formatted historical_os cols
historical_os <- historical_os[,c(1:10, 12)]

colnames(historical_os) <- c('Date', 'Away', 'Away_Score', 'Home', 'Home_Score',
                             'Result', 'Home_Spread',  'ATS', 'Total', 'OU', 'str_id')


# format dates
historical_os <- filter(historical_os, is.na(Date)==F)
historical_os$Date <- as.Date(historical_os$Date, '%b %d, %Y')
historical_os <- filter(historical_os, Date>'2014-01-01')

min(gs_wide$date)

# Weird team equivalencies
# VAN=MEM, NYJ=BKN, NOH=NOP, PHO=PHX, SEA=OKC
# Map os team names -> gs team names

######
hosh <- historical_os$Home
hosh <- str_replace(hosh, 'CHR', 'CHA')
hosh <- str_replace(hosh, 'GS', 'GSW')
hosh <- str_replace(hosh, 'NOH', 'NOP')
hosh <- str_replace(hosh, 'NY', 'NYK')
hosh <- str_replace(hosh, 'PHO', 'PHX')
hosh <- str_replace(hosh, 'SAN', 'SAS')

aosh <- historical_os$Away
aosh <- str_replace(aosh, 'CHR', 'CHA')
aosh <- str_replace(aosh, 'GS', 'GSW')
aosh <- str_replace(aosh, 'NOH', 'NOP')
aosh <- str_replace(aosh, 'NY', 'NYK')
aosh <- str_replace(aosh, 'PHO', 'PHX')
aosh <- str_replace(aosh, 'SAN', 'SAS')

historical_os$Home <- hosh
historical_os$Away <- aosh

######
gs_wide[1:30,]
gs_wide_home <- filter(gs_wide, home==T)
gs_wide_away <- filter(gs_wide, home==F)

gs_wide_gamewise <- inner_join(gs_wide_home, gs_wide_away, by =c('game_id', 'counts_for_double', 'date', 'multiplier',
                                                                 'attendance', 'stage'), suffix=c('.home', '.away'))
head(gs_wide_gamewise)

# Winner needs to be recalculated, 
gs_wide_gamewise$winner.home <- ifelse(gs_wide_gamewise$points.home>gs_wide_gamewise$points.away, T, F)
gs_wide_gamewise$winner.away <- ifelse(gs_wide_gamewise$points.away>gs_wide_gamewise$points.home, T, F)


######
# Create string_id for historical_os and gs_wide 

historical_os$str_id <- paste(historical_os$Home, historical_os$Away, historical_os$Date, sep ='')
gs_wide_gamewise$str_id <- paste(gs_wide_gamewise$team_code.home, gs_wide_gamewise$team_code.away, gs_wide_gamewise$date, sep='')

######

colnames(historical_os) <- c("date", "team_code.away", "points.away", "team_code.home", "points.home", 
                             "Result", "Home_Spread", "ATS", "Total", "OU", "str_id")
historical_os$points.away <- as.numeric(historical_os$points.away)
historical_os$points.home <- as.numeric(historical_os$points.home)
historical_os$Total <- as.numeric(historical_os$Total)
historical_os$Home_Spread <- as.numeric(historical_os$Home_Spread)

historical_os <- dplyr::distinct(historical_os)

dupes <- duplicated(historical_os$str_id)
sum(dupes)

historical_os_unique <- historical_os[!dupes,]


merged_df <- inner_join(historical_os_unique, gs_wide_gamewise, by=c('date', 'team_code.away',
                                                               'points.away', 'team_code.home',
                                                              'points.home', 'str_id'))
sum_na <- function(x){
  result <- sum(is.na(x))
  return(result)
}

write.csv2(merged_df, file = '~/Fall2020/sport_sett_basketball/merged_data.csv')


merged_df <- arrange(merged_df, date)

### Convert to ToI and Opposition

acro_teams <- sort(unique(merged_df$team_code.home))

past_game_roll_mean <- function(x){
  answer_ <- c(x[1:(length(x)-1)], 0)
  answer <- rollmean(answer_, 10, fill=mean(x), align = 'right')
  return(answer)
}

roll_merged <- function(merged_df){
  first <- T
  for (i in acro_teams){
    df_home <- filter(merged_df, team_code.home == i)
    df_away <- filter(merged_df, team_code.away == i)
    
    colnames(df_home)[grep('*.home$', colnames(df_home))] <- unlist(lapply(colnames(merged_df[,grep('*.home$', colnames(df_home))]), str_replace, pattern='.home', replacement='.ToI'))
    colnames(df_home)[grep('*.away$', colnames(df_home))] <- unlist(lapply(colnames(merged_df[,grep('*.away$', colnames(df_home))]), str_replace, pattern='.away', replacement='.Opp'))
    
    colnames(df_away)[grep('*.away$', colnames(df_away))] <- unlist(lapply(colnames(merged_df[,grep('*.away$', colnames(df_away))]), str_replace, pattern='.away', replacement='.ToI'))
    colnames(df_away)[grep('*.home$', colnames(df_away))] <- unlist(lapply(colnames(merged_df[,grep('*.home$', colnames(df_away))]), str_replace, pattern='.home', replacement='.Opp'))
    
    df_away <- df_away[,colnames(df_home)]
    
    df_tmp <- rbind(df_home, df_away)
    df_tmp[,20:33] <- sapply(df_tmp[,20:33], past_game_roll_mean)
    df_tmp[,37:50] <- sapply(df_tmp[,37:50], past_game_roll_mean)
    
    if (first==T){
      df_ans <- df_tmp
      first <- F
    }
    else{
      df_ans <- rbind(df_ans, df_tmp)
    }
  }
  return(df_ans)
}

unroll_merged_df <- function(merged_df){
  first <- T
  for (i in acro_teams){
    df_home <- filter(merged_df, team_code.home == i)
    df_away <- filter(merged_df, team_code.away == i)
    
    colnames(df_home)[grep('*.home$', colnames(df_home))] <- unlist(lapply(colnames(merged_df[,grep('*.home$', colnames(df_home))]), str_replace, pattern='.home', replacement='.ToI'))
    colnames(df_home)[grep('*.away$', colnames(df_home))] <- unlist(lapply(colnames(merged_df[,grep('*.away$', colnames(df_home))]), str_replace, pattern='.away', replacement='.Opp'))
    
    colnames(df_away)[grep('*.away$', colnames(df_away))] <- unlist(lapply(colnames(merged_df[,grep('*.away$', colnames(df_away))]), str_replace, pattern='.away', replacement='.ToI'))
    colnames(df_away)[grep('*.home$', colnames(df_away))] <- unlist(lapply(colnames(merged_df[,grep('*.home$', colnames(df_away))]), str_replace, pattern='.home', replacement='.Opp'))
    
    df_away <- df_away[,colnames(df_home)]
    
    df_tmp <- rbind(df_home, df_away)
    
    if (first==T){
      df_ans <- df_tmp
      first <- F
    }
    else{
      df_ans <- rbind(df_ans, df_tmp)
    }
  }
  return(df_ans)
}

roll_merged_df <- roll_merged(merged_df)
unroll_merged_df <- unroll_merged_df(merged_df)

roll_merged_df$points.Diff <- roll_merged_df$points.ToI-roll_merged_df$points.Opp
unroll_merged_df$points.Diff <- unroll_merged_df$points.ToI-unroll_merged_df$points.Opp


