# Serie A - Data Engineering - Form
# Creates team season-long records and form variables based on results achieved

# ---- Reading in Data


library(dplyr)
library(tidyr)
library(zoo)

source(file = "~/Personal_Git/Soccer_Prediction/SerieA/Processing_and_Engineering/engineering_seriea_raw_scraped.R")


## Calculating records by Season

# Overall Records - Overall, Home, Away
# Recent Records (Past 5 Matches) - Overall, Home, Away 


# Calcluating Home Winners, Losers, Draws in results dataset

results <- raw_scraped[,c("game_id","season","round","Team_h","Team_a","goals_h","goals_a")] %>% 
  arrange(season,round)


# Calculating winner column, and dummy vars (6) for home and away results  
results <- results %>% 
  mutate(winner=ifelse(goals_h>goals_a,as.character(Team_h),
                       ifelse(goals_a>goals_h,as.character(Team_a),"Draw"))) %>%
  mutate(home_win=ifelse(Team_h==winner,1,0),
         home_draw=ifelse(winner=="Draw",1,0),
         home_loss=ifelse(Team_a==winner,1,0),
         away_win=ifelse(Team_a==winner,1,0),
         away_draw=ifelse(winner=="Draw",1,0),
         away_loss=ifelse(Team_h==winner,1,0))



# ---- Overall, Home, and Away Records as % of Points gained

teams <- unique(results$Team_h)

overall_records_list <- list()
for (i in teams){
  
  home <- results %>% filter(Team_h==i) #%>% group_by(season) %>% summarise(h_wins=sum(home_win))
  away <- results %>% filter(Team_a==i) #%>% group_by(season) %>% summarise(a_wins=sum(away_win))
  
  home_record <- home %>% group_by(season) %>%
    summarise(home_wins=sum(home_win),
              home_draws=sum(home_draw),
              max_home_rounds=n()) %>%
    mutate(season_home_record=(3*home_wins+home_draws)/(max_home_rounds*3))
  away_record <- away %>% group_by(season) %>%
    summarise(away_wins=sum(away_win),
              away_draws=sum(away_draw),
              max_away_rounds=n()) %>%
    mutate(season_away_record=(3*away_wins+away_draws)/(max_away_rounds*3)) 
  
  total_record <- left_join(home_record,away_record)  
  
  total_record <- total_record %>%
    mutate(season_record=((3*(home_wins+away_wins)+(home_draws+away_draws))/((max_home_rounds+max_away_rounds)*3))) %>%
    select(season,season_record)
  
  
  home_record <- home_record %>% select(season,season_home_record)
  away_record <- away_record %>% select(season,season_away_record)
  
  Overall_total_record<- left_join(total_record,home_record)
  Overall_total_record<- left_join(Overall_total_record,away_record)
  
  Overall_total_record$Team <- i
  
  overall_records_list[[i]] <- Overall_total_record
  
  rm(home,away,home_record,away_record,total_record,Overall_total_record)
  
}


season_records_df <- bind_rows(overall_records_list)
rm(overall_records_list)

# ---- Calcluating "Form" Previous 5 matches  

# makes use of rollmeanr from the 'zoo' package

team_form_records <- list() 
for (i in teams){
  ind_team_records <- results %>% filter(Team_a==i|Team_h==i)
  ind_team_records$Team<-i
  
  ind_team_records$Venue <- ifelse(ind_team_records$Team_h==i,"Home","Away")
  
  ind_team_records$goals_scored <- ifelse(ind_team_records$Team_h==i,
                                          ind_team_records$goals_h,
                                          ind_team_records$goals_a)
  
  ind_team_records$goals_conceded <- ifelse(ind_team_records$Team_h==i,
                                            ind_team_records$goals_a,
                                            ind_team_records$goals_h)
  
  ind_team_records$points <- ifelse(ind_team_records$winner==i,3,
                                    ifelse(ind_team_records$winner=="Draw",1,0))
  
  # Trailing form for last 5 games
  ind_team_records <- ind_team_records %>% 
    arrange(game_id) %>%
    group_by(season) %>% 
    mutate(form_all=zoo::rollmeanr(lag(points,1),5,fill=NA)/3)
  
  # Trailing form for last 5 HOME games  
  ind_team_records_home <- ind_team_records %>% 
    arrange(game_id) %>%
    filter(Team_h==i) %>%
    group_by(season) %>% 
    mutate(venue_form=zoo::rollmeanr(lag(points,1),5,fill=NA)/3) %>%
    select(game_id,venue_form)
  
  
  # Trailing form for last 5 AWAY games
  ind_team_records_away <- ind_team_records %>% 
    arrange(game_id) %>%
    filter(Team_a==i) %>%
    group_by(season) %>% 
    mutate(venue_form=zoo::rollmeanr(lag(points,1),5,fill=NA)/3) %>%
    select(game_id,venue_form)
  
  ind_team_venue_records <- rbind(ind_team_records_home,ind_team_records_away)
  
  ind_team_records <- left_join(ind_team_records,ind_team_venue_records)
  
  team_form_records[[i]] <- ind_team_records 
  
  rm(ind_team_records_away,ind_team_records_home,ind_team_venue_records)
}

team_records_df<-bind_rows(team_form_records) %>%
  dplyr::select(season,round,game_id,Team,Venue,goals_scored,goals_conceded,points,form_all,venue_form) %>%
  as.data.frame()

rm(team_form_records,results)

# Around 13% of the form_all records fall within round 1:4 and are NA 

# Impute first 4 records of form_all for each team with last season's overall form
# Overall_total_record
team_form_records_to_impute <- left_join(team_records_df,season_records_df) %>%
  filter(round%in%1:5)


teams<-unique(team_form_records_to_impute$Team)
seasons<-unique(team_form_records_to_impute$season)
imputed_form <- list()
for (i in teams){
  temp<-team_form_records_to_impute %>% filter(Team==i)
  temp$form_all <- ifelse(temp$season=="2018-19",
                          subset(temp,season=="2017-18")$season_record[1],
                          ifelse(temp$season=="2017-18",
                                 subset(temp,season=="2016-17")$season_record[1],
                                 ifelse(temp$season=="2016-17",
                                        subset(temp,season=="2015-16")$season_record[1],temp$form_all)))
  
  # imputing hoem form records
  temp$venue_form <- ifelse(temp$season=="2018-19"&temp$Venue=="Home",
                            subset(temp,season=="2017-18"&Venue=="Home")$season_home_record[1],
                            ifelse(temp$season=="2017-18"&temp$Venue=="Home",
                                   subset(temp,season=="2016-17"&Venue=="Home")$season_home_record[1],
                                   ifelse(temp$season=="2016-17"&temp$Venue=="Home",
                                          subset(temp,season=="2015-16"&Venue=="Home")$season_home_record[1],temp$venue_form)))
  
  # imputing away form records  
  temp$venue_form <- ifelse(temp$season=="2018-19"&temp$Venue=="Away",
                            subset(temp,season=="2017-18"&Venue=="Away")$season_away_record[1],
                            ifelse(temp$season=="2017-18"&temp$Venue=="Away",
                                   subset(temp,season=="2016-17"&Venue=="Away")$season_away_record[1],
                                   ifelse(temp$season=="2016-17"&temp$Venue=="Away",
                                          subset(temp,season=="2015-16"&Venue=="Away")$season_away_record[1],temp$venue_form)))
  
  imputed_form[[i]]<-temp
  rm(temp)
}


imputed_form_df<-bind_rows(imputed_form) 


# imputed_form_df has records for 204 of 320 rows

rm(imputed_form,team_form_records_to_impute)

# Integrating imputed data

team_records_df <- team_records_df %>% filter(!game_id%in%imputed_form_df$game_id)
team_records_df <- left_join(team_records_df,season_records_df)

team_records_df <- rbind(team_records_df,imputed_form_df) %>% arrange(game_id)

rm(imputed_form_df,ind_team_records,season_records_df,i,seasons,teams)

