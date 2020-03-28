########## Processing Winter '19

## Notes

# Processing and Engineering
  # Remove features which don't have predictive power
  # Combine features where it makese sense (combining attacking on Left,Right,Center)
  # Create new features (form, ranking, match_id)
  # Re-create performance features as trailing ones (e.g. Total Shots to Avg. Shots over previous 3 matches)

# Data Manipulation
  # Created df's with match data for each individual team 
  # Remove stats that don't have to do with opponents performance (e.g. shots_h for away matches) 


### STILL DO TO: ADD in ELO Data


## Set Up ----

load(file = "./2019 Winter Approach/seriea_2018_19.rdata")
load(file = "./2019 Winter Approach/archive_serie.rdata")

library(tidyverse)
library(randomForest)
library(rpart)


## Processing Step #1 ----

raw <- rbind(seriea_2018_19,archive)

# Processing season, round, outcome, adding match_id, combining attack channels into 1 variables
raw$season <- factor(raw$season,
                     levels=c("2015-16","2016-17",
                              "2017-18","2018-19"))
raw$round <- as.integer(raw$round)

raw$outcome <- ifelse(raw$goals_h>raw$goals_a,"H",
                      ifelse(raw$goals_h<raw$goals_a,"A",
                      ifelse(raw$goals_a==raw$goals_h,"D",NA)))
raw$outcome <- factor(raw$outcome)

raw <- raw %>% arrange(season,round)

match_ids <- raw %>% 
  select(season,round,Team_h,Team_a) %>% 
  unique() %>%
  ungroup %>%
  mutate(match_id=row_number())

raw <- left_join(raw,match_ids)

raw$attacks_h <- raw$attacks_left_h + raw$attacks_middle_h + raw$attacks_right_h
raw$attacks_a <- raw$attacks_left_a + raw$attacks_middle_a + raw$attacks_right_a

raw <- raw %>% select(-c(attacks_left_a,attacks_left_h,attacks_middle_a,attacks_middle_h,attacks_right_a,attacks_right_h))


## Feature Selection with a Random Forest ----

# Variable Importance Plot
raw_to_filter <- raw %>% select(-season,-round,-goals_h,-goals_a,-Team_h,-Team_a,match_id)

Filter_Forest <- randomForest(outcome ~ ., data=raw_to_filter)
importance(Filter_Forest)
varImpPlot(Filter_Forest,main = "Feature Importance in Predicting Match Outcome ")

# Single Tree Viz
SingleTree <- rpart(outcome ~ ., data=raw_to_filter)
plot(SingleTree)
text(SingleTree)

# Variables to Drop
VI <- as.data.frame(importance(Filter_Forest))
VI$feature <- row.names(VI)
VI %>% arrange(-MeanDecreaseGini) # Will remove red, shots_off, pen, shots_sp, fk_shots (on+off), yellow

Variables_To_Drop <- c("red_a","red_h","shot_off_fk_h","shot_off_fk_a",
                       "shot_on_fk_a","shot_on_fk_h","shots_sp_off_a","shots_sp_off_h",
                       "shots_sp_on_h","shots_sp_on_a","yellow_h","yellow_a","pen_h","pen_a")

# removing 'unimportant' variables, drops from 14 features
subset_raw <- raw %>% select(-Variables_To_Drop)


## Processing Step #2 ----

## Need to re-create data as datasets for each individual team
# Also
## Feature Engineering Tasks
  # Create a form variable
  # Create a ranking variable
  # Re-create all features as trailing

team_master_list <- unique(raw$Team_h) %>% as.character()
Ind_team_data <- list()


for(i in 1:length(team_master_list)){
  
  ### Break apart data into list of df's with one for each team
  
  tmp_team <- team_master_list[[i]]
  tmp_data <- subset_raw %>% filter(Team_a==tmp_team|Team_h==tmp_team)
  tmp_data <- tmp_data %>% arrange(season,round)
  
  ### Calculating Points gained per match
  tmp_data$Team_name <- tmp_team
  tmp_data$Team_points <- ifelse(tmp_data$outcome=="D",1,
                                 ifelse(tmp_data$outcome=="H" & tmp_data$Team_h==tmp_team,3,
                                 ifelse(tmp_data$outcome=="A" & tmp_data$Team_a==tmp_team,3,0)))
  
  ## Calculating Form Based off previous 6 matches (not including current)
    # will use the same formula below for trailing metrics
  
  tmp_data$Team_form <- zoo::rollapply(
    data = tmp_data$Team_points,
    width= 7,
    FUN = function(x) sum(x[-7],na.rm=T)/18,
    align="right", fill=NA)


  
  # inputting NAs for first four matches of each season
  tmp_data$Team_form <- ifelse(tmp_data$round%in%1:6,NA,tmp_data$Team_form)
  
  
  ### Outputting into list
  Ind_team_data[[i]] <- tmp_data
}



## Each record in the list contains stats for both the team and the opponent team ("_h" & "_a")
# Want to re-create list of dfs with only data for the individual team
# Re-creating numeric features as trailing variables
# will join the opponents stats by 'match_id' afterwards


Ind_team_data_2 <- list()

for(j in 1:length(Ind_team_data)){
  
  tmp_ind_df <- Ind_team_data[[j]]
  tmp_team_name <- tmp_ind_df$Team_name %>% unique()
  
  # isolating home records
  df_home_records <- tmp_ind_df %>% 
    filter(Team_h==tmp_team_name) %>% 
    mutate(Venue="Home") %>%
    select(match_id,Team_name,season,round,Venue,outcome,Team_points,Team_form,ends_with("_h"),Team_a) %>%
    select(-Team_h)
  
  # isolating away records  
  df_away_records <- tmp_ind_df %>% 
    filter(Team_a==tmp_team_name) %>% 
    mutate(Venue="Away") %>%
    select(match_id,Team_name,season,round,Venue,outcome,Team_points,Team_form,ends_with("_a"),Team_h) %>%
    select(-Team_a)
  
  # re-establishing names with no "_h" or "_a"
  Constant_names <- c("match_id","Team_name","season","round","Venue","outcome",
                      "Team_points","Team_form","goals","saves","shots","shots_on",
                      "shots_off","shots_box","fouls","scoring_chances","offsides",
                      "corners","balls_lost","balls_won","attacks","Opponent")
  
  names(df_home_records) <- Constant_names
  names(df_away_records) <- Constant_names
  
  df2 <- rbind(df_home_records,df_away_records)

  df2 <- df2 %>% arrange(season,round)
  
  ## Creating Trailing Variables
  # Turning values into running averages of past 6 matches, without including the current match
  
  #zoo::rollapply(data = x, 
  #               width = 7, 
  #               FUN = function(x) mean(x[-7],na.rm=T), 
  #               align = "right", 
  #               fill = NA)
  
  #previous_6_mean <- function(D,W=7){
  #  zoo:rollapply(data=D,
  #                width=W,
  #                FUN = function(x) mean(D[-W], na.rm=T),
  #                align="right", fill=NA)
  #}
  
#  to_trail <- c("goals","saves","shots","shots_on","shots_off","fouls","scoring_chances","offsides","corners",
#                "balls_lost","balls_won","attacks","shots_box")
  
  
  df2$goals <- zoo::rollapply(
    data = df2$goals,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$saves <- zoo::rollapply(
    data = df2$saves,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$shots <- zoo::rollapply(
    data = df2$shots,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$shots_on <- zoo::rollapply(
    data = df2$shots_on,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$shots_off <- zoo::rollapply(
    data = df2$shots_off,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$fouls <- zoo::rollapply(
    data = df2$fouls,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$scoring_chances <- zoo::rollapply(
    data = df2$scoring_chances,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$offsides <- zoo::rollapply(
    data = df2$offsides,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$corners <- zoo::rollapply(
    data = df2$corners,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$balls_lost <- zoo::rollapply(
    data = df2$balls_lost,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$balls_won <- zoo::rollapply(
    data = df2$balls_won,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$attacks <- zoo::rollapply(
    data = df2$attacks,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  df2$shots_box <- zoo::rollapply(
    data = df2$shots_box,
    width= 7,
    FUN = function(x) mean(x[-7],na.rm=T),
    align="right", fill=NA)
  
  # Export
  Ind_team_data_2[[j]] <- df2
  
}
all_records <- bind_rows(Ind_team_data_2)


## Joining records based on Match_ID variable to have home and away stats for each record

Ind_team_data_3 <- list()

for(k in 1:length(Ind_team_data_2)){

  ind_team_df <- Ind_team_data_2[[k]]
  temp_team_name3 <- ind_team_df$Team_name %>% unique()
  
  # isolating temp_team records and all opposing ones by match ID
  team <- all_records %>% filter(Team_name==temp_team_name3)
  opp <- all_records %>% filter(!Team_name==temp_team_name3) %>% 
    filter(match_id%in%team$match_id) %>%
    select(-c(Team_name,season,round,Venue,outcome,Team_points,Opponent))
  
  names(opp) <- c("match_id","Opp_form","Opp_goals","Opp_saves",
                  "Opp_shots","Opp_shots_on","Opp_shots_off","Opp_shots_box",
                  "Opp_fouls","Opp_scoring_chances",
                  "Opp_offsides","Opp_corners","Opp_balls_lost","Opp_balls_won","Opp_attacks")
  
  df_out <- left_join(team,opp)  
  
  Ind_team_data_3[[k]] <- df_out
}

# filtering out the first season
processed <- bind_rows(Ind_team_data_3) %>% filter(!season=="2015-16")

# Recreating some character variables as factors
processed$Team_name <- factor(processed$Team_name)
processed$Venue <- factor(processed$Venue)

save(processed,file="2019 Winter Approach/processed.rdata")

rm(all_records,archive,df_away_records,df_home_records,df_out,df2,Filter_Forest,
   Ind_team_data,Ind_team_data_2,Ind_team_data_3,ind_team_df,match_ids,opp,raw,
   raw_to_filter,seriea_2018_19,SingleTree,subset_raw,tmp_data,tmp_ind_df,VI,
   Constant_names,i,j,k,team_master_list,temp_team_name3,tmp_team,tmp_team_name,
   Variables_To_Drop,team,df,fit,model1,nested_df_3_season,nested_df,results_out,
   rfResults,test,threeSeasonteams,tmp_test,tmp_train,train,trControl,x,tableOut,overallAcc,
   test1,train1)

