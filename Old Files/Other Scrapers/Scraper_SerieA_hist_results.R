## This script gathers La Lia match results for 19 teams from 2000 to 2019.
# Intending to use this historical data as a basis for ELO ratings

library(rvest)
library(tidyverse)
library(stringr)


teams <- c("athletico-bilbao","atletico-madrid","cd-alaves","cd-leganes","celta-vigo",
           "espanyol-barcelona","fc-barcelona","getafe-cf","girona-fc","levante-ud",
           "rayo-vallecano","real-betis","real-madrid","real-sociedad","real-valladolid",
           "sd-eibar","sd-huesca","sevilla-fc","valencia-cf","villarreal-cf")
years <- 2000:2019

clock_start <- Sys.time()

team_output <- list()
year_output <- list()

for (j in teams[13:19]){
  for (i in years){
    
    print(paste("scraping ",j," for ",i," season ..",sep=""))
    
    url <- paste("https://www.worldfootball.net/teams/",j,"/",i,"/3/",sep="")
    
    # inserting pause in scraper
    pause_time<-sample(c(1,2,4),1)
    profvis::pause(pause_time)
    
    # pulling down table
    temp_hist <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="site"]/div[3]/div[1]/div/div[3]/div/table') %>%
      html_table(fill = T) %>%
      as.data.frame() 
    
    # Subsetting table into only Serie A results
    # identifying row with "Serie A"
    x<-grepl("Primera", temp_hist[,"X1"], ignore.case=TRUE)
    begin<-match(T,x)+1
    
    # skipping if there is no Serie A data for the team in that year
    if (is.na(begin)){
      next
    }
    
    # identifying row with "Coppa"
    y<-grepl("Copa", temp_hist[,"X1"], ignore.case=TRUE)
    end<-match(T,y)-1
    
    temp_hist2 <- temp_hist[begin:end,]
    names(temp_hist2) <- temp_hist2[1, ]
    temp_hist2 <- temp_hist2[-1, ]
    
    # Adding year and team columns
    temp_hist2$year <-i
    temp_hist2$team <- j 
    
    temp_hist2$Team_goals <- substr(temp_hist2$Results,1,1)
    temp_hist2$Opp_goals <- substr(temp_hist2$Results,3,3)
    
    temp_hist2$outcome <- ifelse(temp_hist2$Team_goals==temp_hist2$Opp_goals,"Draw",
                                 ifelse(temp_hist2$Team_goals<temp_hist2$Opp_goals,"Opp","Team"))
    
    temp_hist2$points <- ifelse(temp_hist2$outcome=="Draw",1,
                          ifelse(temp_hist2$outcome=="Team",3,
                          ifelse(temp_hist2$outcome=="Opp",0,NA))) 
      
    # removing irrelevant columns
    temp_hist2 <- temp_hist2[, -c(5,8)]
      
    year_output[[i]]<-temp_hist2 
  }   
  
  team_output[[j]] <- year_output[years]
}


# full<-c(first14,team_output)
# glimpse(full)



full_team_df <- list()
for (i in teams){
  tmp <- team_output[[i]]
  full_team_df[[i]]<-bind_rows(tmp)
}

final_df<-bind_rows(full_team_df)

final_df <-final_df %>% 
  filter(!Round%in%c("Final","Replays","Round","Serie A 1999/2000 Playoff","Serie A 2004/2005 Relegation")) %>%
  select(-date.1)

final_df$date <- as.Date(final_df$date, format="%d/%m/%Y")

## Recoding final_df_full$Round into final_df_full$Round_clean
final_df_full$Round_clean <- final_df_full$Round
final_df_full$Round_clean[final_df_full$Round == "1. Round"] <- "1"
final_df_full$Round_clean[final_df_full$Round == "2. Round"] <- "2"
final_df_full$Round_clean[final_df_full$Round == "3. Round"] <- "3"
final_df_full$Round_clean[final_df_full$Round == "4. Round"] <- "4"
final_df_full$Round_clean[final_df_full$Round == "5. Round"] <- "5"
final_df_full$Round_clean[final_df_full$Round == "6. Round"] <- "6"
final_df_full$Round_clean[final_df_full$Round == "7. Round"] <- "7"
final_df_full$Round_clean[final_df_full$Round == "8. Round"] <- "8"
final_df_full$Round_clean[final_df_full$Round == "9. Round"] <- "9"
final_df_full$Round_clean[final_df_full$Round == "10. Round"] <- "10"
final_df_full$Round_clean[final_df_full$Round == "11. Round"] <- "11"
final_df_full$Round_clean[final_df_full$Round == "12. Round"] <- "12"
final_df_full$Round_clean[final_df_full$Round == "13. Round"] <- "13"
final_df_full$Round_clean[final_df_full$Round == "14. Round"] <- "14"
final_df_full$Round_clean[final_df_full$Round == "15. Round"] <- "15"
final_df_full$Round_clean[final_df_full$Round == "16. Round"] <- "16"
final_df_full$Round_clean[final_df_full$Round == "17. Round"] <- "17"
final_df_full$Round_clean[final_df_full$Round == "18. Round"] <- "18"
final_df_full$Round_clean[final_df_full$Round == "19. Round"] <- "19"
final_df_full$Round_clean[final_df_full$Round == "20. Round"] <- "20"
final_df_full$Round_clean[final_df_full$Round == "21. Round"] <- "21"
final_df_full$Round_clean[final_df_full$Round == "22. Round"] <- "22"
final_df_full$Round_clean[final_df_full$Round == "23. Round"] <- "23"
final_df_full$Round_clean[final_df_full$Round == "24. Round"] <- "24"
final_df_full$Round_clean[final_df_full$Round == "25. Round"] <- "25"
final_df_full$Round_clean[final_df_full$Round == "26. Round"] <- "26"
final_df_full$Round_clean[final_df_full$Round == "27. Round"] <- "27"
final_df_full$Round_clean[final_df_full$Round == "28. Round"] <- "28"
final_df_full$Round_clean[final_df_full$Round == "29. Round"] <- "29"
final_df_full$Round_clean[final_df_full$Round == "30. Round"] <- "30"
final_df_full$Round_clean[final_df_full$Round == "31. Round"] <- "31"
final_df_full$Round_clean[final_df_full$Round == "32. Round"] <- "32"
final_df_full$Round_clean[final_df_full$Round == "33. Round"] <- "33"
final_df_full$Round_clean[final_df_full$Round == "34. Round"] <- "34"
final_df_full$Round_clean[final_df_full$Round == "35. Round"] <- "35"
final_df_full$Round_clean[final_df_full$Round == "36. Round"] <- "36"
final_df_full$Round_clean[final_df_full$Round == "37. Round"] <- "37"
final_df_full$Round_clean[final_df_full$Round == "38. Round"] <- "38"


final_df_full <- final_df_full %>% select(-Round)

final_df_full <- final_df_full[,c(1,13,2:12)]

names(final_df_full)[2] <- "Round"

#final_df_full <- rbind(final_df_12,final_df)
save(final_df_full,file="Data//serieAhist.rdata")

clock_end <- Sys.time()
clock_end - clock_start # Takes 21 minutes


final_df_full %>% 
  group_by(team) %>% 
  summarise(all_points=sum(points), game_count=n()) %>% 
  mutate(perc=all_points/(game_count*3)*100) %>% 
  arrange(-perc) 

# viz over time 
final_df_full<- final_df_full %>% 
  arrange(team,date) %>% 
  group_by(team) %>% 
  mutate(cum_points = cumsum(points))

ggplot(final_df_full, aes(x=date,y=cum_points, colour=team)) +
  geom_line() + theme_minimal() + ylab("Total Points") + xlab("Date")
  






