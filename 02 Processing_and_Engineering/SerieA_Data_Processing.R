library(randomForest)
library(tidyverse)


#### Importing Data ----

load(file="00 Data/italy_elos.rdata")
italy_elos$Club <- as.factor(italy_elos$Club)

load(file="00 Data/archive1920.rdata")
load(file="00 Data/archive_seriea_18_19.rdata")
load(file="00 Data/archive_seriea_1516_1718.rdata")

df_raw <- rbind(archive,archive1819,archive1920)

rm(archive,archive1819,archive1920)

#### Processing Raw Data ----

# combining attack variables, adding match_id name
df_raw <- df_raw %>% 
  mutate(attacks_h=(attacks_middle_h+attacks_left_h+attacks_right_h),
         attacks_a=(attacks_middle_a+attacks_left_a+attacks_right_a)) %>% 
  select(-c(attacks_middle_h,attacks_middle_a,attacks_left_a,attacks_left_h,
            attacks_right_a,attacks_right_h)) %>% 
  arrange(match_date) %>%
  mutate(match_id=row_number())

# converting season into factor
df_raw$season <- factor(df_raw$season,
                        levels=c("2015-16","2016-17",
                                 "2017-18","2018-19","2019-20"))

# creating outcome variable
df_raw$outcome <- ifelse(df_raw$goals_h>df_raw$goals_a,"H",
                      ifelse(df_raw$goals_h<df_raw$goals_a,"A",
                             ifelse(df_raw$goals_a==df_raw$goals_h,"D",NA)))

# converting outcome into factor
df_raw$outcome <- factor(df_raw$outcome)

# Removing any columns with NA (due to changing variables collected season by season)
df_raw <- df_raw[ , colSums(is.na(df_raw)) == 0]

save(df_raw,file="00 Data/full_raw_scraped.rdata")


#### Feature Selection using Random Forest ----

# Variable Importance Plot
raw_to_filter <- df_raw %>% 
  select(-season,-round,-goals_h,-goals_a,-Team_h,-Team_a,-match_id,-match_date)

Filter_Forest <- randomForest(outcome ~ ., data=raw_to_filter)
importance(Filter_Forest)
varImpPlot(Filter_Forest,
           main = "Feature Importance in Predicting Match Outcome",n.var = ncol(raw_to_filter)-1)

Variables_To_Drop <- c("pen_h","pen_a","shot_off_fk_a","shot_off_fk_h",
                       "shot_on_fk_h","shot_on_fk_a","shots_sp_on_h","shots_sp_on_a",
                       "shots_sp_off_h","shots_sp_off_a")

# removing 'unimportant' variables, drops from 14 features
df_raw <- df_raw %>% select(-Variables_To_Drop)

rm(Filter_Forest,raw_to_filter,Variables_To_Drop)


#### Breaking into team based dataframes ----

team_names <- df_raw$Team_h %>% unique()

team_dfs <- list()

for(i in 1:length(team_names)){
  
## Isolating team records
  tmp_team <- team_names[[i]] %>% as.character()
  
  home_records <- df_raw %>% filter(Team_h==tmp_team)
  away_records <- df_raw %>% filter(Team_a==tmp_team)
  
  ## Adding home_match info
  home_records$home_match <- 1
  away_records$home_match <- 0

## Renaming home and away columns into team/opp    
  home_records_names <- names(home_records)
  home_records_names <- stringr::str_replace(home_records_names,"_h$","_team")
  home_records_names <- stringr::str_replace(home_records_names,"_a$","_opp")
  names(home_records) <- home_records_names
  
  away_records_names <- names(away_records)
  away_records_names <- stringr::str_replace(away_records_names,"_a$","_team")
  away_records_names <- stringr::str_replace(away_records_names,"_h$","_opp")
  names(away_records) <- away_records_names
  
  

    
## Reordering away_records to match
  away_records <- away_records %>% select(home_records_names)

## Binding records together  
  team_records <- rbind(home_records,away_records) %>% 
    arrange(match_date) %>% as.data.frame()
  
  team_records$outcome <- ifelse(team_records$goals_team>team_records$goals_opp,"W",
                                 ifelse(team_records$goals_team<team_records$goals_opp,"L","D"))
  
  team_records$Points_gained <- ifelse(team_records$outcome=="W",3,
                                    ifelse(team_records$outcome=="D",1,
                                    ifelse(team_records$outcome=="L",0,NA)))
  
## Adding Elos for team
  team_elos <- italy_elos %>% 
    filter(Club==tmp_team)
  
  team_records <- left_join(team_records,team_elos, c("Team_team"="Club")) %>%
    filter(match_date>=From) %>%
    filter(match_date<=To) %>%
    select(-To,-From)
  
## Removing anything ending in opp (will re-join by match-id after trailing step)
  team_records <- team_records %>% select(-ends_with("opp"))

  team_dfs[[i]] <- team_records
  
  rm(away_records,home_records,team_elos,team_records,home_records_names,away_records_names,tmp_team)
  
}



#### Trailing variables by 3 matches----

trailing_func <- function(data,trail_count){
  lag(zoo::rollmean(x=data,k=trail_count,align="right",fill=NA),1)
  # Example
  # y <-c(10,10,10,13,7,0)
  # lag(zoo::rollmean(x = y,k=3,align="right",fill=NA),1)
}

team_trailing <- list()

# Setting Trailing amount to 3
trail_count_iter<-3

for(k in 1:length(team_dfs)){
  
  tmp_df <- team_dfs[[k]]
  
  trail_df <- tmp_df %>% 
    group_by(Team_team,season) %>%
    mutate(goals_team=trailing_func(goals_team,trail_count_iter),
           saves_team=trailing_func(saves_team,trail_count_iter),
           shots_team=trailing_func(shots_team,trail_count_iter),
           shots_on_team=trailing_func(shots_on_team,trail_count_iter),
           shots_off_team=trailing_func(shots_off_team,trail_count_iter),
           shots_box_team=trailing_func(shots_box_team,trail_count_iter),
           fouls_team=trailing_func(fouls_team,trail_count_iter),
           scoring_chances_team=trailing_func(scoring_chances_team,trail_count_iter),
           offsides_team=trailing_func(offsides_team,trail_count_iter),
           corners_team=trailing_func(corners_team,trail_count_iter),
           yellow_team=trailing_func(yellow_team,trail_count_iter),
           fast_breaks_team=trailing_func(fast_breaks_team,trail_count_iter),
           poss_team=trailing_func(poss_team,trail_count_iter),
           attacks_team=trailing_func(attacks_team,trail_count_iter))
    
  team_trailing[[k]] <- trail_df
  
}


### Re-creating and joining opposition data and betting data----

load(file="00 Data/betting_probabilities.rdata")


master_trailing <- bind_rows(team_trailing) %>% as.data.frame() %>% arrange(match_id)
last <- ncol(master_trailing)
colnames(master_trailing)[last] <- "Elo_team"

# Exactly 2 copies of each match_id
# master_trailing %>% group_by(match_id) %>% tally() %>% select(n) %>% unique()

# creating opposition data
opp_trailing <- master_trailing

opp_names <- names(opp_trailing)
opp_names <- stringr::str_replace(opp_names,"_team$","_opp")
names(opp_trailing) <- opp_names

# Removing home_match from opp 
opp_trailing <- opp_trailing %>% select(-home_match)


final_data <- list()

teams <- master_trailing$Team_team %>% unique()
for (f in 1:length(teams)){
  
  tmp_team <- teams[f]
  
  # isolating team records
  team_records <- master_trailing %>% filter(Team_team==tmp_team) 
  
  # finding matching records for opposition
  opp_records <- opp_trailing %>% 
    filter(match_id%in%team_records$match_id) %>% 
    filter(!Team_opp==tmp_team) %>%
    select(-outcome,-Points_gained)
  
  final_records <- left_join(team_records,opp_records) 
  
  # Re-ordering columns
  final_records <- final_records[,c(19,17,14,15,1,24,21,20,22,2:13,16,18,23,25:39)]
  
  # cleaning up team column names
  names(final_records)[5] <- "Team"
  names(final_records)[6] <- "Opp"
  
  # Chaning characters to factors
  final_records$Team <- as.factor(final_records$Team)
  final_records$Team <- as.factor(final_records$Team)
  final_records$home_match <- as.factor(final_records$home_match)
  final_records$outcome <- as.factor(final_records$outcome)
  
  # Adding in betting odds
  team_probs <- team_betting_probs %>% filter(Team==tmp_team) %>% select(Team,Opp,B365_team,B365_opp,B365D,home_match,season) 
  team_probs$home_match <- as.factor(team_probs$home_match)
  
  final_records2 <- left_join(final_records,team_probs,by=c("Team"="Team","Opp"="Opp","home_match"="home_match","season"="season"))
  
    
  final_data[[f]] <- final_records2
  
}

save(final_data,file="00 Data/Team_split_Data.rdata")

