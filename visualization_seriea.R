# Visualization

library(dplyr)
library(ggplot2)
library(scales)
library(ggcorrplot)

source(file="engineering_seriea_design_matrix.R")

# Plotting Chances home, away vs results
ggplot(DF, aes(x=scoring_chances_h, y=scoring_chances_a, color=result)) + 
  geom_jitter(size=2.5, alpha=0.5) +
  theme_minimal() +
  labs(title="Home, Away Chances and Results") +
  xlab("Scoring Chances - Home") + ylab("Scoring Chances - Away")


# Plotting shots on target
ggplot(DF, aes(x=DF$shots_on_h, y=DF$shots_on_a, color=result)) + 
  geom_jitter(size=2.5, alpha=0.5) +
  theme_minimal() +
  labs(title="Home, Away Shots on Target and Results") +
  xlab("SoT - Home") + ylab("SoT - Away")



# Correlation of Attacking Home Metrics
home_attack_stats <- DF %>% 
  select(`Home Team`,ends_with("_h"),starts_with("h_")) 

home_attack_stats <- home_attack_stats[complete.cases(home_attack_stats), ]

cor <- cor(home_attack_stats[, sapply(home_attack_stats, is.numeric)])
p.cor <- cor_pmat(home_attack_stats[, sapply(home_attack_stats, is.numeric)])

ggcorrplot(cor,  type = "upper", outline.col = "white", hc.order = TRUE, p.mat = p.cor) +
  labs(title = "Correlation of Attack Stats (H)")




team_record_viz<-team_records_df %>% select(season,round,Team,Venue,points,form_all,venue_form) %>% as.data.frame()

team_record_viz$results <-ifelse(team_record_viz$points==3,"W",
                                 ifelse(team_record_viz$points==1,"D","L"))
team_record_viz$results <- factor(team_record_viz$results,levels = c("W","D","L"))

# re-stating round variable as a numeric so the plots work
team_record_viz$round <- as.numeric(team_record_viz$round)

# Plotting Team Form 
teams <-unique(team_record_viz$Team)
team_form_vizs <- list()
for (i in teams){
  temp<-team_record_viz %>% filter(Team==i)
  teamTitle <- paste(i,"Overall Form by Season") 
  
  temp_viz<-ggplot(temp, aes(x=round,y=form_all)) + 
    geom_line() + 
    geom_point(data=temp, aes(x=round, y=points/3, color=results)) + 
    scale_color_manual(values = c("#31a354","#bdbdbd","#e34a33")) +
    facet_wrap(vars(season),ncol = 1) + 
    theme_minimal() + xlab("Match Day") + ylab("Form - % of Points taken from Last 5 Matches") + 
    ggtitle(teamTitle) +
    theme(axis.text.y = element_blank(), legend.position = "bottom")
  
  team_form_vizs[[i]]<-temp_viz
}

rm(teams,temp,teamTitle)
# Show all plots
for (i in 1:26){plot(team_form_vizs[[i]])}
  # Add season ending point total and or ranking?


# Visualizing Attacks and Defensive 

