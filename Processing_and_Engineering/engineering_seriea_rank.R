# Serie A - Data Engineering - Ranking
# Creates weekly rankings of teams based upon previous 38 matches 

source(file = "~/Personal_Git/Soccer_Prediction/SerieA/Processing_and_Engineering/engineering_seriea_form.R")

# calcluating average form for the previous 38 matches
team_records_df<-team_records_df %>% 
  group_by(Team) %>% 
  mutate(form_hist_38=zoo::rollmeanr(lag(points,1),38,fill=NA)/3)

# calculating ranks based on season and round. 
  # Better teams have lower ranks (e.g. Napoli is generally ranked 2)
team_records_df<-team_records_df %>% 
  group_by(season,round) %>% 
  mutate(league_rank=rank(-form_hist_38, ties.method = "first",na.last = TRUE)) 

# Ensuring any records from 2015-16 season have NA for rank
team_records_df$league_rank <- ifelse(team_records_df$season=="2015-16",
                                      NA,team_records_df$league_rank)