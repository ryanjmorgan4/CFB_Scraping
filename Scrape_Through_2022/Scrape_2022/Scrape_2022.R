###### Load libraries

library(tidyverse)
library(rvest)
library(lubridate)
library(doParallel)
library(foreach)

#First, let's read in the current csv files (up through the 2021 Season).

Game_Logs <- read.csv(file="/Users/Ryan/Desktop/CFB_DATA/Game_Logs_Updated.csv", stringsAsFactors = F)


Game_Results <- read.csv(file="/Users/Ryan/Desktop/CFB_DATA/Game_Results_Updated.csv", stringsAsFactors = F) 


Individual_Season_Results <- read.csv(file="/Users/Ryan/Desktop/CFB_DATA/Individual_Season_Results_updated.csv")


Season_Averages <- read.csv(file="/Users/Ryan/Desktop/CFB_DATA/Season_Averages_Updated.csv") %>% unique()

Total_Team_History <- read.csv(file="/Users/Ryan/Desktop/CFB_DATA/Total_Team_History_2021.csv")



#Do some spot checking on the files you just read in

Game_Logs %>% filter(Team == "southern-california", Season == 2021)
Game_Results %>% filter(Team == "nebraska", Season == 2021)
Individual_Season_Results %>% filter(Team == "nebraska")
Season_Averages %>% filter(Team == "nebraska", Season == 2021)
Total_Team_History %>% filter(Team == "nebraska")


#We now have everything as of 2021, so we now want to update it to be up through the 2022 Season.


#Update Total Team History:

asNumber <- function(dataframe, columns){
  temp= dataframe
  i = 1
  for(i in columns){
    temp[,i] = as.numeric(paste(dataframe[,i]))
  }
  return(temp)
}


nameFormat <- function(originalNames){
  ans <- tolower(originalNames)
  ans <- gsub(" ","-",ans)
  ans <- gsub("[()]*","",ans)
  ans <- gsub("[&]","",ans)
  ans <- gsub("--","-",ans)
  ans <- gsub("'","",ans)
  
  nameReplace <- data.frame(old= c("lsu","smu","ole-miss","pitt$","uab","ucf","usc",
                                   "utep","utsa","^louisiana$","byu", "^bowling-green$", 
                                   "the-citadel", "tcu", "^sam-houston$", "^west-texas-state$"), 
                            new= c("louisiana-state","southern-methodist","mississippi",
                                   "pittsburgh","alabama-birmingham","central-florida", 
                                   "southern-california","texas-el-paso","texas-san-antonio","louisiana-lafayette","brigham-young", "bowling-green-state",
                                   "citadel", "texas-christian" ,"sam-houston-state", "west-texas-am"))
  
  numReplace <- length(nameReplace$old)
  
  for(i in 1:numReplace){
    ans<- gsub(nameReplace$old[i],nameReplace$new[i],ans)
  }
  return(ans)
}

yearsActive <- function(team){
  data =read_school_history(team)$Year
  
  return(data)
}



team_history_totals <- function(){
  url = "http://www.sports-reference.com/cfb/schools/"
  nodes <- "td"
  ans <- read_html(url) %>% html_nodes(nodes) %>% html_text()
  ans <- as.data.frame(matrix(ans, ncol= 19, byrow =T))
  colnames(ans) = c("Team","First_Year","Last_Year","Num_Seasons","Games","Wins","Losses","Ties","Winning_Pct","Bowl_Games","Bowl_Wins","Bowl_Losses","Bowl_Ties","Bowl_Winning_Pct","SRS","SOS", "Years_Ranked_In_AP","Conference_Championships","Notes")
  ans$Team = nameFormat(ans$Team)
  ans = asNumber(ans, c(2:18))
  return(ans)
}

Total_Team_History <- team_history_totals()


#Update Individual Season Results

read_school_history <- function(team){
  team <- nameFormat(team)
  
  url = paste("http://www.sports-reference.com/cfb/schools/",team,"/", sep="")
  
  
  nodesText <- paste("poptip,td,#",team,"_clone a",sep="")
  
  
  schoolHistory <- read_html(url) %>% html_nodes(nodesText) %>% html_text()
  
  
  #if first date is higher than 2013, allow for version that has CFP rankings
  
  if(schoolHistory[1] > "2013"){
    schoolHistory <- as.data.frame(matrix(data=schoolHistory,ncol=20, byrow=T))
    
    
    
    colnames(schoolHistory) = c("Year","Conf","W","L","T","Pct","Conf W", "Conf L", "Conf T", "Conf Pct","SRS","SOS","AP_Pre","AP_High","AP_Post","CFP_High", "CFP_Final","Coach(es)","Bowl","Notes")
    
    schoolHistory$Bowl_Result = gsub(".*\\(", "", schoolHistory$Bowl)
    
    schoolHistory$Bowl_Result = gsub("W","Win",schoolHistory$Bowl_Result)
    
    schoolHistory$Bowl_Result = gsub("L","Loss",schoolHistory$Bowl_Result)
    
    schoolHistory$Bowl_Result = gsub("T","Tie",schoolHistory$Bowl_Result)
    
    schoolHistory$Bowl_Result = gsub(")","",schoolHistory$Bowl_Result)
    
    schoolHistory$Bowl <- gsub(" \\(.*", "", schoolHistory$Bowl)
    
    schoolHistory$Team = rep(team,length(schoolHistory[,1]))
    
    
    schoolHistory = asNumber(schoolHistory,c(1,3,4,5,6,7,8,9,10,11, 12, 13, 14,15, 16, 17))
    
    
    schoolHistory <- schoolHistory[c(22, 1:19,21,20)]
  }
  
  
  else{
  
    
    schoolHistory <- as.data.frame(matrix(data=schoolHistory,ncol=18, byrow=T))
    
    
    
    colnames(schoolHistory) = c("Year","Conf","W","L","T","Pct","Conf W", "Conf L", "Conf T", "Conf Pct","SRS","SOS","AP_Pre","AP_High","AP_Post","Coach(es)","Bowl","Notes")
    
    schoolHistory$Bowl_Result = gsub("[[:print:]]*[-]","",schoolHistory$Bowl)
    
    schoolHistory$Bowl_Result = gsub("W","Win",schoolHistory$Bowl_Result)
    
    schoolHistory$Bowl_Result = gsub("L","Loss",schoolHistory$Bowl_Result)
    
    schoolHistory$Bowl_Result = gsub("T","Tie",schoolHistory$Bowl_Result)
    
    schoolHistory$Bowl = gsub("[-][WLT]","",schoolHistory$Bowl)
    
    schoolHistory$Team = rep(team,length(schoolHistory[,1]))
    
    
    schoolHistory = asNumber(schoolHistory,c(1,3,4,5,6,7,8,9,10,11, 12, 13, 14,15))
    
    
    schoolHistory <- schoolHistory[c(20, 1:17,19,18)]
    
    schoolHistory$CFP_High = NA
    schoolHistory$CFP_Final = NA
    
  }
  schoolHistory
}


#We want to read in the "School History" pages for every team available. I wrote a mult_history function to use the read_school_history function on multiple schools.

mult_history <- function(teams){
  numteams = length(teams)
  
  output = read_school_history(teams[1])
  
  i=2
  
  for(i in 2:numteams){
    output <- rbind(output, read_school_history(teams[i]))
    print(teams[i])
    Sys.sleep(10)
    
  }
  
  output
  
}



#I then ran the mult_history function on the TeamList list of 301 teams.


TeamList <- Total_Team_History$Team


Individual_Season_Results_Updated<- mult_history(TeamList)


length(unique(Individual_Season_Results_Updated$Team))




#Now update team stats


read_team_stats <- function(team,year){

  Sys.sleep(15)
  
  team <- nameFormat(team)
  
  url=paste("http://www.sports-reference.com/cfb/schools/",team,"/",year,".html",sep="")
  
  teamStats <- read_html(url) %>% html_nodes("#team td") %>% html_text()
  
  teamStats <- as.data.frame(matrix(teamStats,ncol=22, byrow = T))
  
  colnames(teamStats) =c("Games","Completions","Pass_Attempts","Completion_Percentage","Passing_Yards","Passing_TD","Rushing_Attempts","Rushing_Yards","Rushing_Avg","Rushing_TD","Plays","Total_Yards","Yards_Per_Play","Pass_First_Downs","Rush_First_Downs","Penalty_First_Downs","Total_First_Downs","Number_Of_Penalties","Penalty_Yards","Fumbles","Interceptions","Turnovers")
  
  
  
  teamStats <- asNumber(teamStats, c(1:22))
  
  teamStats$Season = rep(year,3)
  teamStats$Team = rep(team, 3)
  
  conf <- paste((read_school_history(team) %>% filter(Year==year)%>% select(Conf))[1,1])
  
  teamStats$Conference = rep(conf, 3)
  
  teamStats <- teamStats[c(24,23,25,1:22)]
  
  teamStats$Type = c("Offense","Defense","Difference")
  
  teamStats <- teamStats[c(1:3,26,4:25)]
  
  teamStats
}


read_team_stats(team="nebraska", year = 2022)


#get a list of teams who played in 2022:
  


teams_2022 <- Individual_Season_Results_Updated %>% filter(Year == 2022)

teams_2022 <- teams_2022$Team

teams_2022 <- nameFormat(teams_2022)



update_2022 <- function(teams){
  
  numteams = length(teams)
  
  output = read_team_stats(teams[1], 2022)
  
  i=2
  
  for(i in 2:numteams){
    
    output <- bind_rows(output, read_team_stats(teams[i], 2022))
    print(teams[i])
  }
  
  output
}


season_averages_2022 <- update_2022(teams_2022)
length(unique(season_averages_2022$Team))


#Schedule results

read_schedule_results <- function(team,year){
  
  Sys.sleep(15)
  
  team <- nameFormat(team)
  
  if(year>1995){
    url=paste("http://www.sports-reference.com/cfb/schools/",team,"/",year,"-schedule.html",sep="")
    
    scheduleResults <- read_html(url) %>% html_nodes(".right, .left") %>% html_text()
    
    scheduleResults = scheduleResults[-length(scheduleResults)]
    scheduleResults = scheduleResults[-length(scheduleResults)]
    
    scheduleResults <- as.data.frame(matrix(scheduleResults,ncol=14,byrow = T))
    
    colnames(scheduleResults) = c("Game_Number","Date","Day","Team","Location","Opponent","OpponentConf","Result","Points","Opponent_Points","TotalWins","TotalLosses","Streak","Notes")
    
  }
  
  else{
    url=paste("http://www.sports-reference.com/cfb/schools/",team,"/",year,"-schedule.html",sep="")
    
    scheduleResults <- read_html(url) %>% html_nodes(".right, .left") %>% html_text()
    
    scheduleResults = scheduleResults[-length(scheduleResults)]
    scheduleResults = scheduleResults[-length(scheduleResults)]
    
    scheduleResults <- as.data.frame(matrix(scheduleResults,ncol=15,byrow = T))
    
    colnames(scheduleResults) = c("Game_Number","Date","Day","Team","Location","Opponent","OpponentConf","Result","Points","Opponent_Points","TotalWins","TotalLosses","del","Streak","Notes")
    
    scheduleResults = scheduleResults %>% select(-del)
    
  }
  
  #FIX THE DATE COLUMN
  
  scheduleResults$Date <- mdy(scheduleResults$Date)
  
  #USE PARSING TO GET A COLUMN OF TEAM RANKINGS
  
  scheduleResults$TeamRank= as.numeric(gsub(".*?([0-9]+).*", "\\1", scheduleResults$Team))
  
  
  #Make it so the school column is just the name, not the rankings.
  
  scheduleResults$Team = rep(team, length(scheduleResults$Date))
  
  
  
  #CREATE A COLUMN TO SAY EITHER "HOME" "AWAY" OR "NEUTRAL"
  
  
  scheduleResults$Location <- gsub("N","Neutral",scheduleResults$Location)
  
  
  scheduleResults$Location <- gsub("@","Away",scheduleResults$Location)
  
  
  temp = scheduleResults$Location
  
  temp[temp==""] = "Home"
  
  scheduleResults$Location = temp
  
  #CREATE COLUMN WITH OPPONENT RANK
  
  scheduleResults$OpponentRank <-  as.numeric(gsub(".*?([0-9]+).*", "\\1", scheduleResults$Opponent))
  
  
  #Make it so the opponent column is just the name, not the rankings.
  
  scheduleResults$Opponent = gsub("[(][[:digit:]]*[)]","",scheduleResults$Opponent)
  
  
  #You'll have to fix the " " at the beginning of ranked opponents names.
  
  scheduleResults$Opponent = gsub(intToUtf8(160),"",scheduleResults$Opponent)
  
  #Fix the formatting of team names
  
  scheduleResults$Team = nameFormat(scheduleResults$Team)
  
  scheduleResults$Opponent = nameFormat(scheduleResults$Opponent)
  
  #Create a column with the season
  
  scheduleResults$Season = rep(year, length(scheduleResults$Opponent))
  
  
  #USE THE ASNUMBER FUNCTION TO MAKE COLUMNS NUMBERS.
  
  scheduleResults <- asNumber(scheduleResults, c(1,9:12,15,16))
  
  conf <- paste((read_school_history(team) %>% filter(Year==year)%>% select(Conf))[1,1])
  
  scheduleResults$Conference = rep(conf, length(scheduleResults$Team))
  
  scheduleResults <- scheduleResults[c(4,17,18,1,2,3,5,15,6,16,7,8,9:14)]
  
  
  return(scheduleResults)
  
  
}


team_schedule_update_2022<- function(teams){
  
  numteams = length(teams)
  
  output = read_schedule_results(teams[1], 2022)
  
  i=2
  
  for(i in 2:numteams){
    output <- rbind(output, read_schedule_results(teams[i], 2022))
    print(teams[i])
    
  }
  
  output
  
}

schedule_results_2022 <- team_schedule_update_2022(teams_2022)
length(unique(schedule_results_2022$Team))

#Update Game Logs


OffensegameLogs <- function(team,season){
  team<- nameFormat(team)
  
  Sys.sleep(15)
  
  url <- paste("http://www.sports-reference.com/cfb/schools/",team,"/",season,"/gamelog/",sep="")
  
  gameLog <- read_html(url) %>% html_nodes("tr td") %>% html_text
  
  
  gameLog <- as.data.frame(matrix(gameLog,ncol=25,byrow=T))
  
  colnames(gameLog) = c("Date","Location","Opponent","Result","Pass_Completions","Pass_Attempts","Passing_Pct","Pass_Yards","Pass_TD","Rush_Attempts","Rush_Yards","Rush_Avg","Rush_TD","Total_Plays","Total_Yards","Yards_per_play","Pass_first_downs","Rush_first_downs","Penalty_first_downs","Total_first_downs","Penalties","Penalty_yards","Fumbles","Interceptions","Turnovers")
  
  
  #Get rid of the last row (the totals)
  
  numRows = length(rownames(gameLog))
  
  gameLog <- gameLog[-numRows,]
  
  
  #Fix the Date Column
  
  gameLog$Date = ymd(gameLog$Date)
  
  
  #Fix Location Column
  
  gameLog$Location <- gsub("N","Neutral",gameLog$Location)
  
  
  gameLog$Location <- gsub("@","Away",gameLog$Location)
  
  
  temp = gameLog$Location
  
  temp[temp==""] = "Home"
  
  gameLog$Location = temp
  
  
  #Fix Opponent Column
  
  gameLog$Opponent = nameFormat(gameLog$Opponent)
  
  gameLog$Opponent = gsub("\\*","",gameLog$Opponent)
  
  gameLog$Opponent = nameFormat(gameLog$Opponent)
  
  # Fix the Result column. We want either a "Win" or "Loss" under the "Result Column." We will also want A column with "Team Points" and "Opponent Points"
  
  
  temp = gameLog$Result
  
  result = temp
  
  result=gsub("[[:digit:]]*","",result)
  
  result=gsub("[\\(\\)-]*","",result)
  
  result=gsub("\\s","",result)
  
  gameLog$Result=result
  
  team_Score = temp
  
  team_Score = gsub("[LW]*\\s*[(]*","",team_Score)
  
  team_Score = gsub("-[[:digit:]]*\\)","",team_Score)
  
  gameLog$Points_Scored = team_Score
  
  opp_points = temp
  
  opp_points = gsub("[LW]*\\s*[(]*","",opp_points)
  
  opp_points = gsub("[[:digit:]]*-","",opp_points)
  
  opp_points = gsub("\\)","",opp_points)
  
  gameLog$Opp_points = opp_points
  
  
  #Make sure necesarry columns are number format
  
  gameLog = asNumber(dataframe = gameLog,columns = c(5:27))
  
  #Make a "Team" column
  
  gameLog$Team=rep(team,length(gameLog$Date))
  
  #Make a "Season" column
  
  gameLog$Season = rep(season,length(gameLog$Date))
  
  
  #Put the columns in a logical order
  
  gameLog = gameLog[c(28,29,1:4,26,27,5:25)]
  
  #Return
  
  return(gameLog)
  
}

mult_team_offenseLogs_2022 <- function(teams){
  numteams = length(teams)
  
  output = OffensegameLogs(teams[1],2022)
  
  i=2
  
  for(i in 2:numteams){
    output <- bind_rows(output, OffensegameLogs(teams[i],2022))
    print(teams[i])
    
  }
  
  output
  
}



offense_2022_logs <- mult_team_offenseLogs_2022(teams_2022)
length(unique(offense_2022_logs$Team))

offense_2022_logs_save <- offense_2022_logs

offense_2022_logs <- offense_2022_logs_save




#Now the defense logs part:

opponents_2022<-unique(offense_2022_logs$Opponent)

problemOpponents_2022 <- sort(opponents_2022[!opponents_2022 %in%  nameFormat(teams_2022)])



teams_2022 <- nameFormat(teams_2022)

opponents_2022<-unique(offense_2022_logs$Opponent)

problemOpponents_2022 <- sort(opponents_2022[!opponents_2022 %in% teams_2022])

problemGames_2022 <- offense_2022_logs %>% filter(Opponent %in% problemOpponents_2022)

dim(problemGames_2022)


manually_Enter_2022 <- problemGames_2022

manually_Enter_2022$Team = problemGames_2022$Opponent

manually_Enter_2022$Opponent = problemGames_2022$Team

manually_Enter_2022$Points_Scored = problemGames_2022$Opp_points

manually_Enter_2022$Opp_points = problemGames_2022$Points_Scored


## Flip Locations ##
temp= problemGames_2022$Location

temp[problemGames_2022$Location == "Home"] = "Away"

temp[problemGames_2022$Location =="Away"] = "Home"

manually_Enter_2022$Location = temp

## Flip Result ##

temp= problemGames_2022$Result

temp[problemGames_2022$Result=="W"] = "L"

temp[problemGames_2022$Result =="L"] = "W"

manually_Enter_2022$Result = temp


manually_Enter_2022[,9:29] = NA



#I will then write this file as a CSV where I will enter all those annoying offense logs for those teams...


write.csv(manually_Enter_2022,"manual_Games2022.csv")



#read it back in:
  

ManualGames_2022 <- read.csv("manual_Games2022_completed.csv")

ManualGames_2022$Date = mdy(ManualGames_2022$Date)

ManualGames_2022$Team = nameFormat(ManualGames_2022$Team)

ManualGames_2022$Opponent = nameFormat(ManualGames_2022$Opponent)



#Now bind it up:

offense_2022_logs <- bind_rows(offense_2022_logs, ManualGames_2022)

offense_2022_logs <- unique(offense_2022_logs)

read_defense_log <- function(team, year, offenselogs = offense_2022_logs){
  
  team = nameFormat(team)
  
  offenseStats <- offenselogs %>% filter(Team==team, Season==year)
  
  opponentList <- offenseStats$Opponent
  
  dateList <- offenseStats$Date
  
  numOpponents <- length(opponentList)
  
  defenseLog <- data.frame()
  
  i=1
  
  
  for(i in 1:numOpponents){
    newLog = offenselogs %>% filter(Team==opponentList[i],Date == dateList[i])
    
    defenseLog = bind_rows(defenseLog, newLog)
  }
  
  return(defenseLog)
  
}


All_Defense_Log <- function(teamList, offenseLogs= OffenseLogs){
  numTeams = length(teamList)
  
  i = 1
  
  defenseLogs = data.frame()
  
  for(i in 1:numTeams){
    new = read_defense_log(teamList[i],2022)
    
    defenseLogs = bind_rows(defenseLogs, new)
    
  }
  
  return(defenseLogs)
}



#Now make the defense logs:
  
  
  

defense_2022_logs <- All_Defense_Log(teams_2022)


#Throw it all together into game logs:
  
  

colnames(offense_2022_logs) = c("Team","Season","Date","Location","Opponent","Result","Points_Scored","Opponent_Points_Scored","Offensive_Pass_Completions","Offensive_Pass_Attempts","Offensive_Passing_Pct","Offensive_Pass_Yards","Offensvie_Pass_TDs","Offensive_Rush_Attempts","Offensive_Rush_Yards","Offensive_Rush_Avg","Offensive_Rush_TD","Offensive_Total_Plays","Offensive_Total_Yards","Offensive_Yards_Per_Play","Offensive_Pass_First_Downs","Offensive_Rush_First_Downs","Offensive_Penalty_First_Downs","Offensive_Total_First_Downs","Penalties_Committed","Penalty_Yards_Against", "Offensive_Fumbles","Offensive_Interceptions","Turnovers_Against")


colnames(defense_2022_logs) = c("Team","Season","Date","Location","Opponent","Result","Points_Scored","Opponent_Points_Scored","Defensive_Pass_Completions","Defensive_Pass_Attempts","Defensive_Passing_Pct","Defensive_Pass_Yards","Defensive_Pass_TDs","Defensive_Rush_Attempts","Deffensive_Rush_Yards","Defensive_Rush_Avg","Defensive_Rush_TD","Defensive_Total_Plays","Defensive_Total_Yards","Defensive_Yards_Per_Play","Defensive_Pass_First_Downs","Defensive_Rush_First_Downs","Defesnive_Penalty_First_Downs","Defensive_Total_First_Downs","Penalties_For","Penalty_Yards_For","Defensive_Fumbles","Defensive_Interceptions","Turnovers_For")



defense_2022_logs = defense_2022_logs[c(1:3,5,9:29)]

temp = defense_2022_logs$Team

defense_2022_logs$Team= defense_2022_logs$Opponent

defense_2022_logs$Opponent = temp

offense_2022_logs<- offense_2022_logs %>% filter(Team %in% teams_2022)


gameLogs_2022 <- left_join(offense_2022_logs, defense_2022_logs)


#Making sure each row is unique:
  

dim(offense_2022_logs)

dim(unique(offense_2022_logs))

dim(defense_2022_logs)

dim(unique(defense_2022_logs))

anti_join(offense_2022_logs, defense_2022_logs)

OLog <- offense_2022_logs[,7:29]

DLog <- defense_2022_logs[,5:25]

dim(OLog)

dim(unique(OLog))

dim(DLog)

dim(unique(DLog))





#Now clean up and organize the data frame by organizing by Team and then Date. 



gameLogs_2022 <- gameLogs_2022 %>% arrange(Team, Date)







anti_join(schedule_results_2022,gameLogs_2022)


# From here down it'll be project specific


#Replace liu opponent with long-island

schedule_results_2022$Opponent = gsub("^liu$", "long-island", schedule_results_2022$Opponent)

anti_join(schedule_results_2022,gameLogs_2022)



#Replace houston-christian opponent with houston-baptist

gameLogs_2022$Opponent = gsub("^houston-christian$", "houston-baptist", gameLogs_2022$Opponent)

anti_join(schedule_results_2022,gameLogs_2022)




colnames(schedule_results_2022) <- c("Team","Season","Conf","Game_Number","Date","Day","Location","TeamRank","Opponent","OpponentRank","OpponentConf","Result","Points_Scored","Opponent_Points_Scored","CurrentWins","CurrentLosses","CurrentStreak","Notes")

schedule_results_2022$Date <- ymd(schedule_results_2022$Date)

gameLogs_2022 <- left_join(schedule_results_2022, gameLogs_2022)




#Re-do the cleaning:
  
  

colnames(Total_Team_History)[c(2,3,4,5,10,17,15,16)] <- c("First_Season","Last_Season","Number_Of_Seasons","Games_Played","Bowl_Games_Played","Number_Of_Seasons_Ranked_In_AP_Final_Poll","Simple_Rating_System","Strength_Of_Schedule")

colnames(Total_Team_History)


which(is.na.data.frame(Total_Team_History),arr.ind=TRUE)


#Most of those NA's are for the Bowl winning percentage. The others are for the new teams (James Madison, Sam Houston State, and Jacksonsville state)
#Change the NA's to 0's.


Total_Team_History[is.na.data.frame(Total_Team_History)] <- 0

which(is.na.data.frame(Total_Team_History),arr.ind=TRUE)



Total_Team_History <- Total_Team_History %>% arrange(Team)

write.csv(file = "Total_Team_History_2022.csv",Total_Team_History, row.names = F)





colnames(Individual_Season_Results_Updated) <- c("Team","Season","Conference",
                                                 "Wins","Losses","Ties","Winning_Pct",
                                                 "Conference_Wins","Conference_Losses","Conference_Ties","Conference_Winning_Pct", 
                                                 "Simple_Rating_System","Strength_Of_Schedule",
                                                 "AP_Poll_Preseason_Rank","AP_Poll_Highest_Rank","AP_Poll_Postseason_Rank",
                                                 "CFP_Highest_Rank","CFP_Final_Rank",
                                                 "Coach(es)","Bowl_Game", "Bowl_Result", "Notes")

colnames(Individual_Season_Results_Updated)


which(is.na.data.frame(Individual_Season_Results_Updated),arr.ind=TRUE)

#It appears that a lot of the NA values are when a Team wasn't ranked in the AP poll or CFP poll. If there is an NA value for one of the rankings, I will replace it with an "Unranked"


Individual_Season_Results_Updated$AP_Poll_Preseason_Rank[is.na(Individual_Season_Results_Updated$AP_Poll_Preseason_Rank)]<- "Unranked"

Individual_Season_Results_Updated$AP_Poll_Preseason_Rank <- as.factor(Individual_Season_Results_Updated$AP_Poll_Preseason_Rank)

Individual_Season_Results_Updated$AP_Poll_Postseason_Rank[is.na(Individual_Season_Results_Updated$AP_Poll_Postseason_Rank)] <- "Unranked"

Individual_Season_Results_Updated$AP_Poll_Postseason_Rank <- as.factor(Individual_Season_Results_Updated$AP_Poll_Postseason_Rank)


Individual_Season_Results_Updated$AP_Poll_Highest_Rank[is.na(Individual_Season_Results_Updated$AP_Poll_Highest_Rank)] <- "Unranked"

Individual_Season_Results_Updated$AP_Poll_Highest_Rank <- as.factor(Individual_Season_Results_Updated$AP_Poll_Highest_Rank)


Individual_Season_Results_Updated$CFP_Highest_Rank[is.na(Individual_Season_Results_Updated$CFP_Highest_Rank)] <- "Unranked"

Individual_Season_Results_Updated$CFP_Highest_Rank <- as.factor(Individual_Season_Results_Updated$CFP_Highest_Rank)



Individual_Season_Results_Updated$CFP_Final_Rank[is.na(Individual_Season_Results_Updated$CFP_Final_Rank)] <- "Unranked"

Individual_Season_Results_Updated$CFP_Final_Rank <- as.factor(Individual_Season_Results_Updated$CFP_Final_Rank)





which(is.na.data.frame(Individual_Season_Results_Updated),arr.ind=TRUE)


# The conference wins, losses, ties, and winning percentages are NA when a team isn't in a conference.
# Overwrite these with 0s

Individual_Season_Results_Updated$Conference_Wins[is.na(Individual_Season_Results_Updated$Conference_Wins)]<- 0
Individual_Season_Results_Updated$Conference_Losses[is.na(Individual_Season_Results_Updated$Conference_Losses)]<- 0
Individual_Season_Results_Updated$Conference_Ties[is.na(Individual_Season_Results_Updated$Conference_Ties)]<- 0
Individual_Season_Results_Updated$Conference_Winning_Pct[is.na(Individual_Season_Results_Updated$Conference_Winning_Pct)]<- 0


which(is.na.data.frame(Individual_Season_Results_Updated),arr.ind=TRUE)



Individual_Season_Results_Updated[2527, ]
Individual_Season_Results_Updated[8610, ]


Individual_Season_Results_Updated[12680, ]
Individual_Season_Results_Updated[13477, ]


#Apparently 1968 West Virginia doesn't have a simple rating system or a strength of scheudle, and neither does 1937 virginia

#Also, UCONN and old diminion opted out of 2020, so they have NAs.

newConfs<- Individual_Season_Results_Updated$Conference

newConfs <- gsub(pattern = "Pac-8","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-10","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-12","Pac-8/10/12",newConfs)

Individual_Season_Results_Updated$Conference = newConfs


#Finally Sort

Individual_Season_Results_Updated<- Individual_Season_Results_Updated %>% arrange(Team, desc(Season))

write.csv(file="Individual_Season_Results_updated.csv",Individual_Season_Results_Updated, row.names=F)



colnames(season_averages_2022)[c(6:16,18:22)]<-c("Pass_Comp","Pass_Att","Pass_Pct","Pass_Yds","Pass_TD","Rush_Att","Rush_Yds","Rush_Avg","Rush_TD","Total_Plays","Total_Yds","Pass_First_Down","Rush_First_Down","Penalty_First_Down","Total_First_Down","Penalties")

which(is.na.data.frame(season_averages_2022),arr.ind=TRUE)

#Lets replace the NA values in the Games column with 0. 

season_averages_2022$Games[is.na(season_averages_2022$Games)] <- 0

table(which(is.na.data.frame(season_averages_2022),arr.ind=TRUE)[,2])


#Fix PAC conference

newConfs <- season_averages_2022$Conference

newConfs <- gsub(pattern = "Pac-8","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-10","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-12","Pac-8/10/12",newConfs)

season_averages_2022$Conference = newConfs



#Sort

season_averages_updated <- rbind(Season_Averages, season_averages_2022)

season_averages_updated <- season_averages_updated %>% arrange(Team, desc(Season))

write.csv(file="Season_Averages_Updated.csv",season_averages_updated, row.names = F)



colnames(schedule_results_2022)[c(8,10,11,13:17)]<- c("Team_Rank","Opponent_Rank","Opponent_Conference","Points_Scored","Opponent_Points_Scored","Current_Wins","Current_Losses","Current_Streak")


which(is.na.data.frame(schedule_results_2022),arr.ind=TRUE)

#Change columns 8 and 10, so that the NA values are "Unranked"

schedule_results_2022$Team_Rank[is.na(schedule_results_2022$Team_Rank)] <- "Unranked"

schedule_results_2022$Team_Rank <- as.factor(schedule_results_2022$Team_Rank)


schedule_results_2022$Opponent_Rank[is.na(schedule_results_2022$Opponent_Rank)] <- "Unranked"

schedule_results_2022$Opponent_Rank <- as.factor(schedule_results_2022$Opponent_Rank)


which(is.na.data.frame(schedule_results_2022),arr.ind=TRUE)


newResults <- schedule_results_2022$Result

newResults <- gsub(pattern = "W", replacement = "Win", newResults)

newResults <- gsub(pattern = "L", replacement = "Loss", newResults)

newResults <- gsub(pattern = "T", replacement = "Tie", newResults)

newResults <- as.factor(newResults)

schedule_results_2022$Result = newResults


#Fix Team Conference

colnames(schedule_results_2022)[3]= "Conference"
newConfs <- schedule_results_2022$Conference

newConfs <- gsub(pattern = "Pac-8","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-10","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-12","Pac-8/10/12",newConfs)

schedule_results_2022$Conference = newConfs


#Fix Opponent Conference 

newConfs <- schedule_results_2022$Opponent_Conference

newConfs <- gsub(pattern = "Pac-8","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-10","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-12","Pac-8/10/12",newConfs)

schedule_results_2022$Opponent_Conference = newConfs


Game_Results$Date = paste(Game_Results$Date)

schedule_results_2022$Date= paste(schedule_results_2022$Date)

Game_Results_Updated <- rbind(Game_Results, schedule_results_2022)

#Sort

Game_Results_Updated <- Game_Results_Updated %>% arrange(Team, desc(Season), Game_Number)

#Write CSV

write.csv(file = "Game_Results_Updated.csv", Game_Results_Updated, row.names = F)





newResults <- gameLogs_2022$Result

newResults <- gsub(pattern = "W", replacement = "Win", newResults)

newResults <- gsub(pattern = "L", replacement = "Loss", newResults)

newResults <- gsub(pattern = "T", replacement = "Tie", newResults)

newResults <- as.factor(newResults)

gameLogs_2022$Result = newResults


#Fix Team Conference
colnames(gameLogs_2022) = colnames(Game_Logs)
newConfs <- gameLogs_2022$Conference

newConfs <- gsub(pattern = "Pac-8","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-10","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-12","Pac-8/10/12",newConfs)

gameLogs_2022$Conference = newConfs


#Fix Opponent Conference 

newConfs <- gameLogs_2022$Opponent_Conference

newConfs <- gsub(pattern = "Pac-8","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-10","Pac-8/10/12",newConfs)

newConfs <- gsub(pattern = "Pac-12","Pac-8/10/12",newConfs)

gameLogs_2022$Opponent_Conference = newConfs


gameLogs_2022$Date = paste(gameLogs_2022$Date)
Game_Logs$Date = paste(Game_Logs$Date)


table(which(is.na.data.frame(gameLogs_2022),arr.ind=TRUE)[,2])



gameLogs_2022$Team_Rank[is.na(gameLogs_2022$Team_Rank)] <- "Unranked"

gameLogs_2022$Team_Rank <- as.factor(gameLogs_2022$Team_Rank)


gameLogs_2022$Opponent_Rank[is.na(gameLogs_2022$Opponent_Rank)] <- "Unranked"

gameLogs_2022$Opponent_Rank <- as.factor(gameLogs_2022$Opponent_Rank)


gameLogs_2022$Notes[is.na(gameLogs_2022$Notes)] <- ""



gameLogs_2022[which(is.na.data.frame(gameLogs_2022),arr.ind=TRUE)] = 0



#BIND

Game_Logs_Updated <- rbind(Game_Logs, gameLogs_2022)


table(which(is.na.data.frame(Game_Logs_Updated),arr.ind=TRUE)[,2])

#Sort

Game_Logs_Updated <- Game_Logs_Updated %>% arrange(Team, desc(Season), Game_Number)

write.csv(file = "Game_Logs_Updated.csv", Game_Logs_Updated, row.names = F)

