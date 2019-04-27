# engineering design matrix trailing 
library(zoo)

source(file = "~/Personal_Git/Soccer_Prediction/SerieA/Processing_and_Engineering/engineering_seriea_raw_scraped.R",
        verbose = F)
source(file = "~/Personal_Git/Soccer_Prediction/SerieA/Processing_and_Engineering/engineering_seriea_rank.R",
       verbose = F)

# Build a function that calculates the trailing stats for all variables, looping through all teams

teams <- unique(raw_scraped$Team_h) 

trail <- function(x){
  zoo::rollmeanr(lag(x,1),5,fill=NA)/3
}

trail_list <- list()

for (j in 1:length(teams)){
  
  i <- as.character(teams[j])
  
  # breaking apart into home and away
  team_home <- raw_scraped %>% filter(Team_h==i)
  team_away <- raw_scraped %>% filter(Team_a==i)
  
  #subsetting down columns to just those reflecting performance of team i
  team_home <- team_home %>% select(Team_a,game_id,round,season,ends_with("_h")) %>% mutate(Venue="Home")
  colnames(team_home)[1] <- "Opponent"
  team_away <- team_away %>% select(Team_h,game_id,round,season,ends_with("_a")) %>% mutate(Venue="Away")
  colnames(team_away)[1] <- "Opponent"
  
  # remove suffix "_h" or "_a" for columns 5-27 so we can bind the two df's
  for (i in 5:27){
    colnames(team_home)[i] <- substr(colnames(team_home)[i],1,nchar(colnames(team_home)[i])-2)
    colnames(team_away)[i] <- substr(colnames(team_away)[i],1,nchar(colnames(team_away)[i])-2)
      }
  team <- rbind(team_home,team_away) %>% arrange(game_id)
  
  # Calculating trailing averages for all numeric columns
  team_trail <- data.frame(apply(team[ ,6:27], 2, trail))
  
  # subsetting out the non-numeric columns
  team <- team[ ,c(1,2,3,4,28)]
  
  # rebinding to trailing numeric columns
  team <- cbind(team,team_trail)
  
  # Adding form and rank variables from engineering_seriea_rank.R
  team <- left_join(team,team_records_df) %>% 
    select(-goals_scored,-goals_conceded,-points,-season_record,
           -season_home_record,-season_away_record,-form_hist_38)
  
  # Putting Team name at the beginning
  team <- team[, c(28,1:27,29:31)]
  
  trail_list[[j]] <- team 
  
  rm(team_home,team_away,team_trail,team)
}

DF_trailing <- bind_rows(trail_list)
DF_trailing$Team <- factor(DF_trailing$Team)
DF_trailing$Venue <- factor(DF_trailing$Venue)


rm(i,j,teams,trail_list)


  

# Re-creating to all match info in one row (e.g. Team and opponent trailing stats)

teams <- unique(DF_trailing$Team)

DF_trailing_output <- list()

for (j in 1:length(teams)){
  
 i <- as.character(teams[j])  
  
 team_stats <- DF_trailing %>% filter(Team==i)
 opponent_stats <- DF_trailing %>% filter(game_id%in%team_stats$game_id,!Team==i)

 
 # Adding suffixes to both datasets
 for (i in 7:31){
   colnames(team_stats)[i] <- paste(colnames(team_stats)[i],"Team",sep="_")
   colnames(opponent_stats)[i] <- paste(colnames(opponent_stats)[i],"Opp",sep="_")
 }
 
 # Removing all non-numeric variables from opponent_stas except game_id
 opponent_stats <- opponent_stats %>% select(-Team,-Opponent,-round,-season,-Venue)
 
 output <- left_join(team_stats,opponent_stats)
 
 DF_trailing_output[[j]]<-output
 
 rm(opponent_stats,team_stats)
}

DF_trailing_2 <- bind_rows(DF_trailing_output)

rm(DF_trailing,DF_trailing_output,i,j,teams,output)

# Adding in results and converting to Factor for target var
match_results<-team_records_df %>% 
  select(game_id,Team,round,season,Venue,points)

DF_Trailing_3 <-left_join(DF_trailing_2,match_results)
DF_Trailing_3$result <- ifelse(DF_Trailing_3$points==1,"Draw",
                         ifelse(DF_Trailing_3$points==0,"Opp","Team"))

DF_Trailing_3$result <- factor(DF_Trailing_3$result)
DF_Trailing_3$Team <- factor(DF_Trailing_3$Team)
DF_Trailing_3$game_id <- factor(DF_Trailing_3$game_id)


# Filtering out 2015-16 season
DF_trailing <- DF_Trailing_3 %>% filter(!season=="2015-16")



# Cleaning up
rm(DF_Trailing_3,DF_trailing_2,match_results)
