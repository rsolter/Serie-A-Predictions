## This script is just for tears. Gathers Juventus' UCL records since 1997 and visualizes their progress each season.

## UCL Records

library(rvest)
library(tidyverse)
library(data.table)


## Example of grabbing results for 1996
  

juve_ucl_results <- list()
years<-1997:2019

for (i in years){

  print(paste("grabbing for ", i, " season",sep=""))
# Read in tables
  url <- paste("https://www.worldfootball.net/teams/juventus/",i,"/3/",sep="")
  juve_hist001 <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="site"]/div[3]/div[1]/div/div[3]/div/table') %>%
    html_table(fill = T) %>%
    as.data.frame()
  

# grab the first row where Champions League appears to begin subset
  begin <- juve_hist001[juve_hist001$X1 %like% "Champions", ] %>% 
    row.names() %>% as.numeric()
  
  if (length(begin)==0) {
    next
  }

# grab the first row where "Serie A" appears to end subset
  end<-juve_hist001[juve_hist001$X1 %like% "Serie A", ] %>% 
    row.names() %>%
    as.numeric()

# Some seasons have Europea League results -- identifying those
#  second_end <- juve_hist002[juve_hist001$X1 %like% "Europa", ] %>% 
#    row.names() %>% as.numeric()
  
  
#  true_end<-ifelse(length(second_end>0),second_end,end)
  
  juve_hist002<-juve_hist001[(begin+1):(end-1),]
  names(juve_hist002) <- juve_hist002[1,]
  juve_hist002 <- juve_hist002[-1,]
  juve_hist002$year <- i

  
  
  juve_ucl_results[[i]]<-juve_hist002
}
  
juve_ucl_results_final<-juve_ucl_results[years]

fjf<-bind_rows(juve_ucl_results_final)

fjf <- fjf[,c(1,3,5,6,8)]


# Subsetting out UCL Qualifying rounds, europa leagues
UCL_Juve <- fjf[-c(12:15,90:93,112:115,122:127,144:153),]

# isolating scores
UCL_Juve$Home_Score <- substr(UCL_Juve$Results,1,1)
UCL_Juve$Away_Score <- substr(UCL_Juve$Results,3,3)

# coding match outcomes
UCL_Juve$Match_Outcome <- ifelse(UCL_Juve$Home_Score==UCL_Juve$Away_Score,"Draw",
                           ifelse(UCL_Juve$Home_Score>UCL_Juve$Away_Score,"Win",
                           ifelse(UCL_Juve$Home_Score<UCL_Juve$Away_Score,"Loss",
                           #ifelse(UCL_Juve$place=="H"&UCL_Juve$Home_Score>UCL_Juve$Away_Score,"Win",
                           #ifelse(UCL_Juve$place=="H"&UCL_Juve$Home_Score<UCL_Juve$Away_Score,"Loss",
                           #ifelse(UCL_Juve$place=="A"&UCL_Juve$Home_Score<UCL_Juve$Away_Score,"Win",
                           #ifelse(UCL_Juve$place=="A"&UCL_Juve$Home_Score>UCL_Juve$Away_Score,"Loss",
                           ifelse(UCL_Juve$place=="N"&UCL_Juve$year==1997,"Win",
                           ifelse(UCL_Juve$place=="N"&UCL_Juve$year%in%c(1998,2003,2015,2017),"Loss",NA)))))

UCL_Juve <- UCL_Juve %>% group_by(year) %>% mutate(match_num=row_number())
UCL_Juve$points <- ifelse(UCL_Juve$Match_Outcome=="Win",3,
                     ifelse(UCL_Juve$Match_Outcome=="Draw",1,0))

UCL_Juve <- UCL_Juve %>% group_by(year) %>% mutate(Form=zoo::rollmeanr(points,3,fill=NA)) 


# first viz
ggplot(UCL_Juve, aes(x=match_num,y=Form)) + 
  geom_line() +
  geom_point(data=UCL_Juve, aes(x=match_num, y=points, color=Match_Outcome)) + 
  scale_color_manual(values = c("#bdbdbd","#e34a33","#31a354")) +
  facet_wrap(vars(year),ncol = 2) + 
  theme_minimal() + xlab("Match Number") + ylab("Form - % of Points taken from Last 3 Matches") + 
  ggtitle("Juventus UCL Record") +
  theme(axis.text.y = element_blank(), legend.position = "bottom")



# second viz , with stages highlighted
post_2013<-UCL_Juve[68:nrow(UCL_Juve), ]

ggplot(post_2013, aes(x=match_num,y=Form)) + 
  geom_line() +
  geom_point(data=post_2013, aes(x=match_num, y=points, color=Match_Outcome)) + 
  scale_color_manual(values = c("#bdbdbd","#e34a33","#31a354")) +
  annotate("rect", fill = "grey", alpha = 0.3, 
           xmin=0.5,xmax=6.5,ymin=-Inf,ymax=Inf) + 
  annotate("rect", fill = "grey", alpha = 0.15, 
           xmin=6.5,xmax=8.5,ymin=-Inf,ymax=Inf) + 
  annotate("rect", fill = "grey", alpha = 0.3, 
           xmin=8.5,xmax=10.5,ymin=-Inf,ymax=Inf) + 
  annotate("rect", fill = "grey", alpha = 0.15, 
           xmin=10.5,xmax=12.5,ymin=-Inf,ymax=Inf) +
  annotate("rect", fill = "grey", alpha = 0.3, 
           xmin=12.5,xmax=13.5,ymin=-Inf,ymax=Inf) + 
  facet_wrap(vars(year),ncol = 2) + 
  theme_minimal() + xlab("Match Number") + ylab("Form - % of Points taken from Last 3 Matches") + 
  ggtitle("Juventus UCL Record",subtitle = "Shades represent stages (e.g. Group, Round of 16, Qtr-Finals, etc.)") +
  theme(axis.text.y = element_blank(), legend.position = "bottom")





