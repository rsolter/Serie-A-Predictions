# Serie A - Data Engineering
# Creates team records and form variables

# ---- Reading in Data

#start<-proc.time()

library(dplyr)
library(tidyr)
library(zoo)


source(file = "engineering_seriea_rank.R")
source(file="engineering_seriea_raw_scraped.R")


# ---- Combining 'raw_scraped' and 'form' into one dataset 
# Reforamtting from one row per match to two rows for home, away


# Separating out home and away
df_home <- raw_scraped %>% 
  dplyr::select(game_id,season,round,ends_with("_h")) %>% 
  mutate(Venue="Home")

df_away <- raw_scraped %>% 
  select(game_id,season,round,ends_with("_a")) %>% 
  mutate(Venue="Away")

# renaming Team variable
colnames(df_home)[4] <- "Home Team"
colnames(df_away)[4] <- "Away Team"

# remove suffix "_h" or "_a" for columns 5-26
#for (i in 5:26){
#  colnames(df_home)[i] <- substr(colnames(df_home)[i],1,nchar(colnames(df_home)[i])-2)
#  colnames(df_away)[i] <- substr(colnames(df_away)[i],1,nchar(colnames(df_away)[i])-2)
#}

#full_df <- rbind(df_home,df_away)
#rm(df_home,df_away)

form <- team_records_df %>% 
  dplyr::select(season,game_id,round,Team,Venue,points,form_all,venue_form,league_rank)

form$round <- factor(form$round, levels=c("1","2","3","4","5","6","7","8",
                                          "9","10","11","12","13","14","15","16",
                                          "17","18","19","20","21","22","23","24",
                                          "25","26","27","28","29","30","31","32",
                                          "33","34","35","36","37","38"))
form$Team <- factor(form$Team)

df_home <- left_join(df_home,form, by=c("game_id"="game_id",
                                        "season"="season",
                                        "round"="round",
                                        "Venue"="Venue",
                                        "Home Team"="Team"))

df_away <- left_join(df_away,form,by=c("game_id"="game_id",
                                       "season"="season",
                                       "round"="round",
                                       "Venue"="Venue",
                                       "Away Team"="Team"))

colnames(df_away)[28:31] <- c("a_points","a_form_all","a_venue_form","a_league_rank")
colnames(df_home)[28:31] <- c("h_points","h_form_all","h_venue_form","h_league_rank")

df_away <- df_away %>% select(-Venue)
df_home <- df_home %>% select(-Venue)


DF <- left_join(df_home,df_away)

# Establishing factors
DF$game_id <- factor(DF$game_id)
DF$`Home Team` <- factor(DF$`Home Team`)
DF$`Away Team` <- factor(DF$`Away Team`)
DF$`Home Team` <- factor(DF$`Home Team`)

rm(df_away,df_home,form)
## Cleaning up dataset for modeling


DF$result <- ifelse(DF$h_points==3,"Home",
                    ifelse(DF$a_points==3,"Away","Draw"))
DF$result <- factor(DF$result,levels = c("Home","Draw","Away"))

DF <- DF %>% select(-h_points,-a_points)

DF <- DF %>% filter(!season=="2015-16") %>% arrange(game_id)



