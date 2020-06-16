## Betting Data Processing
library(tidyverse)


## Loading 
s1 <- read.csv("00 Data/Betting csvs/I1.csv",header = T,stringsAsFactors = F) %>%
  select(Date,HomeTeam,AwayTeam,B365H,B365A,B365D,BWH,BWD,BWA) %>%
  mutate(season="2019-20")
s2 <- read.csv("00 Data/Betting csvs/I1 (1).csv",header = T,stringsAsFactors = F) %>%
  select(Date,HomeTeam,AwayTeam,B365H,B365A,B365D,BWH,BWD,BWA)%>%
  mutate(season="2018-19")
s3 <- read.csv("00 Data/Betting csvs/I1 (2).csv",header = T,stringsAsFactors = F) %>%
  select(Date,HomeTeam,AwayTeam,B365H,B365A,B365D,BWH,BWD,BWA) %>%
  mutate(season="2017-18")
s4 <- read.csv("00 Data/Betting csvs/I1 (3).csv",header = T,stringsAsFactors = F) %>%
  select(Date,HomeTeam,AwayTeam,B365H,B365A,B365D,BWH,BWD,BWA) %>%
  mutate(season="2016-17")
s5 <- read.csv("00 Data/Betting csvs/I1 (4).csv",header = T,stringsAsFactors = F) %>%
  select(Date,HomeTeam,AwayTeam,B365H,B365A,B365D,BWH,BWD,BWA)%>%
  mutate(season="2015-16")

betting_raw <- rbind(s1,s2,s3,s4,s5)


betting_raw_out <- betting_raw %>% select(season, HomeTeam, AwayTeam, B365H,B365A,B365D)

save(betting_raw_out,file="00 Data/betting_raw.rdata")


#### 

betting_raw$Date <- as.Date(strptime(betting_raw$Date,format="%d/%m/%Y"))

# betting names to be changed: "Verona" -> "HellasVerona", "Chievo" -> "Chieveverona

betting_raw$HomeTeam <- ifelse(betting_raw$HomeTeam=="Verona","HellasVerona",
                               ifelse(betting_raw$HomeTeam=="Chievo","Chievoverona",betting_raw$HomeTeam))

betting_raw$AwayTeam <- ifelse(betting_raw$AwayTeam=="Verona","HellasVerona",
                               ifelse(betting_raw$AwayTeam=="Chievo","Chievoverona",betting_raw$AwayTeam))

table(betting_raw$HomeTeam%in%df_raw$Team_h) # one NA


# Replacing all "odds" with 1/odds to get probabilities

betting_probabilities <- betting_raw %>%
  mutate(B365H=1/B365H,
            B365A=1/B365A,
            B365D=1/B365D,
            BWH=1/BWH,
            BWD=1/BWD,
            BWA=1/BWA)

# Removing one NA record in 2015-16

betting_probabilities[betting_probabilities$HomeTeam=="",]

betting_probabilities <- betting_probabilities %>% filter(!betting_probabilities$HomeTeam=="")


## Regrouping by team

team_names <- betting_probabilities$HomeTeam %>% unique()

team_betting_probs <- list()

for(i in 1:length(team_names)){
  
  ## Isolating team records
  tmp_team <- team_names[[i]] %>% as.character()
  
  home_records <- betting_probabilities %>% filter(HomeTeam==tmp_team)
  away_records <- betting_probabilities %>% filter(AwayTeam==tmp_team)
  
  ## Adding home_match info
  home_records$home_match <- 1
  away_records$home_match <- 0
  
  ## Renaming home and away columns into team/opp    
  home_records_names <- names(home_records)
  home_records_names <- stringr::str_replace(home_records_names,"H$","_team")
  home_records_names <- stringr::str_replace(home_records_names,"A$","_opp")
  home_records_names[[2]] <- "Team"
  home_records_names[[3]] <- "Opp"
  names(home_records) <- home_records_names
  
  away_records_names <- names(away_records)
  away_records_names <- stringr::str_replace(away_records_names,"H$","_team")
  away_records_names <- stringr::str_replace(away_records_names,"A$","_opp")
  away_records_names[[2]] <- "Opp"
  away_records_names[[3]] <- "Team"
  names(away_records) <- away_records_names
  
  # re-ordering away records so the two can be bound
  away_records <- away_records[ ,c(1,3,2,4:11)]
  
  ## Binding records together  
  team_records <- rbind(home_records,away_records) %>% 
    arrange(Date) %>% as.data.frame()
  
  team_records$Date <- as.Date(team_records$Date)

    
  team_betting_probs[[i]] <- team_records
}


team_betting_probs <- team_betting_probs %>% bind_rows()

save(team_betting_probs,file="00 Data/betting_probabilities.rdata")






