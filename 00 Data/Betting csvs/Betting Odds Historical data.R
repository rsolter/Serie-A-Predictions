# Betting Data Processing
library(tidyverse)

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
  mutate(season="2016-27")
s5 <- read.csv("00 Data/Betting csvs/I1 (4).csv",header = T,stringsAsFactors = F) %>%
  select(Date,HomeTeam,AwayTeam,B365H,B365A,B365D,BWH,BWD,BWA)%>%
  mutate(season="2015-16")

betting_raw <- rbind(s1,s2,s3,s4,s5)

# betting names to be changed: "Verona" -> "HellasVerona", "Chievo" -> "Chieveverona

betting_raw$HomeTeam <- ifelse(betting_raw$HomeTeam=="Verona","HellasVerona",
                               ifelse(betting_raw$HomeTeam=="Chievo","Chievoverona",betting_raw$HomeTeam))

betting_raw$AwayTeam <- ifelse(betting_raw$AwayTeam=="Verona","HellasVerona",
                               ifelse(betting_raw$AwayTeam=="Chievo","Chievoverona",betting_raw$AwayTeam))

table(betting_raw$HomeTeam%in%df_raw$Team_h) # one NA
