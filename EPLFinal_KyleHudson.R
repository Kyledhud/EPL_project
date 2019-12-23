library(tidyverse)
library(lubridate)
library(stringr)

# EPL Final Project Function -----------------------------------------------------

EPL_Standings <- function(date, season) {
  first2 <- substr(season, 3, 4)
  last2 <- substr(season, 6, 7)
  year <- paste(first2, last2, sep = "")
  epl_data <- read_csv(url(paste("http://www.football-data.co.uk/mmz4281/", year,"/E0.csv", sep = "")))
  
  # format dates and filter by date input
  epl_filtered <- epl_data %>% 
    select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>%
    mutate(Date = dmy(Date)) %>% 
    filter(Date <= mdy(date))
  
  # assign wins and losses for home and away teams, assign ties (draws), using string count function
  epl_filtered <- epl_filtered %>% 
    mutate(wins = str_count(epl_filtered$FTR, "H"), losses = str_count(epl_filtered$FTR, "A"),
           ties = str_count(epl_filtered$FTR, "D"),
           Awins = str_count(epl_filtered$FTR, "A"), Alosses = str_count(epl_filtered$FTR, "H")) 
  
  # create data frame for home teams
  home <- select(epl_filtered, HomeTeam, FTHG, FTAG, wins, losses, ties) %>%
    group_by(HomeTeam) %>%
    rename(TeamName = HomeTeam) %>% 
    summarize(
      homeGS = sum(FTHG),
      homeGA = sum(FTAG),
      homeWins = sum(wins),
      homeLosses = sum(losses),
      homeTies = sum(ties),
      MatchesPlayed = (sum(homeWins, homeLosses, homeTies)))
  
  # create data frame for away teams
  away <- select(epl_filtered, AwayTeam, FTHG, FTAG, Awins, Alosses, ties) %>% 
    group_by(AwayTeam) %>%
    rename(TeamName = AwayTeam) %>% 
    summarize(
      awayGS = sum(FTAG),
      awayGA = sum(FTHG),
      awayWins = sum(Awins),
      awayLosses = sum(Alosses),
      awayTies = sum(ties),
      MatchesPlayed = (sum(awayWins, awayLosses, awayTies)))
  
  # combine home and away data frames by common column TeamName, create combined stats in this new df  
  teams_combined <- merge(home, away, by = "TeamName") %>% 
    group_by(TeamName) %>% 
    mutate(Wins = homeWins + awayWins,
           Losses = homeLosses + awayLosses,
           Ties = homeTies + awayTies,
           GS = homeGS + awayGS,
           GA = homeGA + awayGA)
  
  # using combined home and away data frame, data frame for all final variables except streak and last10
  team_outcomes <- teams_combined %>%
    group_by(TeamName) %>%
    summarize(Record = str_c(Wins, Losses, Ties, sep = "-"), 
              HomeRec = str_c(homeWins, homeLosses, homeTies, sep = "-"), 
              AwayRec = str_c(awayWins, awayLosses, awayTies, sep = "-"), 
              MatchesPlayed = sum(Wins, Losses, Ties), 
              Points = (Wins * 3) + Ties, 
              PPM = round(Points / MatchesPlayed, digits=2), 
              PtPct = str_c(round(Points / (3 * MatchesPlayed) * 100, digits=2), "%", sep=""),
              GS = GS, 
              GSM = round(GS / MatchesPlayed, digits=2),
              GA = GA, 
              GAM = round(GA / MatchesPlayed, digits=2),
              Wins = Wins)
  
  # begin creating Streak variable
  # assign Team, Date, and FTR to streak home and away data frames
  streak_home <- select(epl_filtered, HomeTeam, Date, FTR) %>% 
    mutate(FTR = ifelse(FTR == "H", "W", 
                        ifelse(FTR == "A", "L", "T")))
  streak_away <- select(epl_filtered, AwayTeam, Date, FTR) %>% 
    mutate(FTR = ifelse(FTR == "H", "L", 
                        ifelse(FTR == "A", "W", "T")))
  
  # change column names to be consistent so home and away streak data frames can be rbinded
  headers <- c("Team", "Date", "Result")
  colnames(streak_home) <- headers
  colnames(streak_away) <- headers
  
  # combine home and away streak data frames using rbind
  epl_streak <- rbind(streak_home, streak_away)
  # order by team and most recent date
  epl_streak <- arrange(epl_streak, Team, desc(Date))
  
  # makes vector of unique team names to iterate through in the following for-loop
  teams <- unique(epl_streak$Team)
  
  # make empty df for streak and last10 data
  results <- data.frame(TeamName = character(), Last10 = character())
  
  # loop through each of the 20 teams
  for(i in teams) {
    
    temp <- epl_streak %>% 
      filter(Team == i)
    
    # assign wins, ties, and losses variables 0 
    # used in second nested for-loop to calculating last10 variable
    wins <- 0
    ties <- 0
    losses <- 0
    
    # variable for most recent game result (W/L/T)
    current_streak <- temp[1,3]
    count <- 1
    
    # streak counter for-loop: iterate through temp to count consecutive W's, L's, or T's
    # if the result does not equal the result of the next row, break and keep the count at it's current value
    # if it does equal, add 1 to the count variable
    for(j in 1:(nrow(temp) - 1)) {
      ifelse(temp$Result[j] != temp$Result[j+1], break, count <- count + 1)
    }
    
    # combine the result of the most recent game (W/L/T) with the streak count
    temp_streak = str_c(current_streak, count, sep="")
    
    # select top 10 rows of temp data frame to calculate last10 variable
    temp <- head(temp, 10)
    
    # last10 for-loop: count W/L/D for the last 10 games of each team
    for (j in 1:nrow(temp)) {
      ifelse(temp$Result[j] == "W", wins <- wins +1, 
             ifelse(temp$Result[j] == "T", ties <- ties + 1, losses <- losses + 1))
    }
    
    # combine W/L/T for the last 10 games, as calculated in above for-loop
    last10_rec <- str_c(wins, losses, ties, sep = '-')
    
    # combine empty "results" df with df containing last10 and streak values for each team
    results <- rbind(results, data.frame(TeamName = i, Last10 = last10_rec, Streak = temp_streak))
    
  }
  
  # combine team outcomes data frame with results (last10 and streak) data frame
  epl_total <- merge(team_outcomes, results, by = "TeamName") %>% 
    # sort total by descending PPM first, then descending Wins, then descending GSM, and lastly GAM
    arrange(desc(PPM), desc((team_outcomes$Wins)), desc(GSM), GAM)
  
  EPL_Final_Results <<- epl_total
  return(EPL_Final_Results)
}

EPL_Standings("01/01/2099", "2017/18")
