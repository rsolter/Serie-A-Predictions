# Reading in Betting Data

bet1819 <- read.csv(file="/home/ravisolter/Personal_Git/Soccer_Prediction/SerieA/Data/football_data_serieA_1819.csv",
                    header = T, stringsAsFactors = F) %>% select(1:4,23:ncol(.)) %>% select(-starts_with("Bb"))
bet1718 <- read.csv(file="/home/ravisolter/Personal_Git/Soccer_Prediction/SerieA/Data/football_data_serieA_1718.csv",
                    header = T, stringsAsFactors = F) %>% select(1:4,23:ncol(.)) %>% select(-starts_with("Bb"))
bet1617 <- read.csv(file="/home/ravisolter/Personal_Git/Soccer_Prediction/SerieA/Data/football_data_serieA_1617.csv",
                    header = T, stringsAsFactors = F) %>% select(1:4,23:ncol(.)) %>% select(-starts_with("Bb"))

# Adding Dates

bet1819$Date <- as.Date(bet1819$Date,format="%d/%m/%Y")
bet1718$Date <- as.Date(bet1718$Date,format="%d/%m/%Y")
bet1617$Date <- as.Date(bet1617$Date,format="%d/%m/%Y")

# Adding Seasons 

bet1819$Season = "2018-19"
bet1718$Season = "2017-18"
bet1617$Season = "2016-17"

# Team List
teams1819 <- c(bet1819$HomeTeam,bet1819$AwayTeam) %>% unique()


# Processing Loop, creates matching df for each team

team_records1819 <- list()

# helper function
last_char <- function(x){
  num_char <-nchar(x)
  y<-substr(x,num_char,num_char)
  return(y)
}



# https://stackoverflow.com/questions/47353073/tidyverse-spread-with-duplicate-keys-and-fill-nas-based-on-unique-keys

for (i in 1:length(teams1819)){
  
  # Isolating one team's records
  tmp_team <- teams1819[i]
  tmp_home_record <- bet1819 %>% dplyr::filter(HomeTeam==tmp_team)
  tmp_away_record <- bet1819 %>% dplyr::filter(AwayTeam==tmp_team)
  tmp_team_record <- rbind(tmp_away_record,tmp_home_record)
  
  
  tmp_team_record <- tmp_team_record %>% 
    arrange(Date) %>% 
    mutate(Round=row_number()) %>% # adding round
    select(-Div) %>%
    mutate(Venue=ifelse(HomeTeam==tmp_team,"Home","Away")) %>% # adding Venue, Team, Opponent
    mutate(Team=tmp_team,
           Opponent=ifelse(HomeTeam==tmp_team,AwayTeam,HomeTeam))
 
  tmp_team_record <- tmp_team_record %>% select(-HomeTeam,-AwayTeam) # Removing old team cols
   
  
  
  long_tmp_team_record <- tmp_team_record %>% 
    gather("BetMaker","Odds",-c(Season,Round,Date,Venue,Team,Opponent)) %>% 
    arrange(Round) 
  long_tmp_team_record$Bet_Outcome <- last_char(long_tmp_team_record$BetMaker)
  long_tmp_team_record$BetMaker <- substr(long_tmp_team_record$BetMaker,1,
                                          nchar(long_tmp_team_record$BetMaker)-1)
  
  
  final_team_record <-long_tmp_team_record %>%
    unite("Place_holder", "Date","Season","Round","Venue","Team","Opponent","BetMaker",sep="_") %>%
    spread(Bet_Outcome,Odds) %>%
    separate(Place_holder,c("Place_holder", "Date","Season","Round","Venue","Team","Opponent","BetMaker"))
  
  final_team_record$A <- 1/final_team_record$A
  final_team_record$D <- 1/final_team_record$D
  final_team_record$H <- 1/final_team_record$H
  
  
  
  team_records1819[[i]] <- final_team_record
  
  
}

glimpse(team_records1819[[1]])


# function to select last element in string var?






