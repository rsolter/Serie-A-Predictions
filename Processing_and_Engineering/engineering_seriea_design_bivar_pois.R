# engineering_bivariate_poisson



## This is a start for estimating lambda for a given team as the estimate
# from the poisson distribution

library(tidyverse)


source(file="engineering_seriea_form.R")
source(file="engineering_seriea_rank.R")
source(file="engineering_seriea_raw_scraped.R")


# Subsetting out some values from team_records_df
team_records_df <- team_records_df %>% select(-points,-goals_scored,-goals_conceded)


# Selecting useful data from raw_scraped
raw_features <- c("Team_h","Team_a","goals_h","goals_a","shots_h","shots_a",
                  "shots_on_h","shots_on_a","shots_box_h","shots_box_a",
                  "scoring_chances_h","scoring_chances_a","balls_lost_h",
                  "balls_lost_a","balls_won_h","balls_won_a","season","round","game_id")

dfP <- raw_scraped %>% select(raw_features)


# Re-creating with twice the number of rows (2 rows per match) 
teams<-unique(dfP$Team_h)

pois_out <- list()

for (j in 1:length(teams)){
  
  i <- as.character(teams[j])
  
  home_temp <- dfP %>% filter(Team_h==i)
  away_temp <- dfP %>% filter(Team_a==i)
  
  #subsetting down columns to just those reflecting performance of team i
  home_temp <- home_temp %>% select(Team_a,game_id,round,season,ends_with("_h")) %>% mutate(Venue="Home")
  colnames(home_temp)[1] <- "Opponent"
  away_temp <- away_temp %>% select(Team_h,game_id,round,season,ends_with("_a")) %>% mutate(Venue="Away")
  colnames(away_temp)[1] <- "Opponent"
  
  # remove suffix "_h" or "_a" for columns 5-11 so we can bind the two df's
  for (i in 5:12){
    colnames(home_temp)[i] <- substr(colnames(home_temp)[i],1,nchar(colnames(home_temp)[i])-2)
    colnames(away_temp)[i] <- substr(colnames(away_temp)[i],1,nchar(colnames(away_temp)[i])-2)
  }
  
  team <- rbind(home_temp,away_temp) %>% arrange(game_id)
  
  # Adding form and rank variables from engineering_seriea_rank.R
  team <- left_join(team,team_records_df)
  #%>% 
  #  select(-goals_scored,-goals_conceded,-points,-season_record,
  #         -season_home_record,-season_away_record,-form_hist_38)

  pois_out[[j]] <- team
  
  rm(team, home_temp, away_temp)
  }


pois_out_df <- bind_rows(pois_out)

rm(i,j,pois_out,raw_features)


# Partition

pois_out_df <- pois_out_df %>% select(-c(Opponent,game_id,round,Team))

pois_out_df_train <- pois_out_df %>% filter(!season=="2018-19") %>% select(-season)
pois_out_df_test <- pois_out_df %>% filter(season=="2018-19") %>% select(-season)

# https://stats.idre.ucla.edu/r/dae/poisson-regression/
poisFit1  <- glm(goals ~ .,family="poisson", data=pois_out_df_train)

summary(poisFit1)

# Running again on those variables with a higher significance
# a_league_rank, scoring chances, shots_box_h, shots_on_h, shots_h, pen_h

poisFit2 <- glm(goals ~ shots + shots_on + shots_box + scoring_chances + balls_lost,
                family="poisson", data=pois_out_df_train)

summary(poisFit2)



## This is a start for estimating lambda for a given team as the estimate
# from the poisson distribution

## The next step would be to get this data in a long format with two rows per team
# as is in DF_trailing

## 


## .. 

### .. Team Name + Trailing goals last 4 matches + Trailing Goals last 38 matches +
##  vs home team ranking, away team ranking, goals conceded by previous team
