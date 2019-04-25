## UCL Records

library(rvest)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/Juventus_F.C._in_European_football"
juv_eu_hist <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = T) %>%
  as.data.frame()


full_juve <- juv_eu_hist


# Final Results
# Subsetting to 1995-96 onwards
juv_eu_hist<-juv_eu_hist[40:nrow(juv_eu_hist), ]
juv_eu_hist$Season <- factor(juv_eu_hist$Season) 
juve_hist<-juv_eu_hist %>% group_by(Season) %>% slice(.,n()) %>% select(Season,Round,Opposition)
juve_hist$points <- ifelse(juve_hist$Round%in%c("Final"),5,
                           ifelse(juve_hist$Round%in%c("Semi-finals","Semifinals","Semi-final"),4,
                                  ifelse(juve_hist$Round%in%c("Quarter-finals","Quarter-final"),3,
                                         ifelse(juve_hist$Round%in%c("Round of 16","First knockout round"),2,1))))


url <- "https://en.wikipedia.org/wiki/Inter_Milan_in_European_football"
inter_eu_hist <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = T) %>%
  as.data.frame()
inter_eu_hist<-inter_eu_hist[27:nrow(inter_eu_hist), ]
inter_eu_hist$Season <- factor(inter_eu_hist$Season) 
inter_hist<-inter_eu_hist %>% group_by(Season) %>% slice(.,n()) %>% select(Season,Round,Opposition)
inter_hist$points <- ifelse(inter_hist$Round%in%c("Final"),5,
                            ifelse(inter_hist$Round%in%c("Semi-finals","Semifinals","Semi-final"),4,
                                   ifelse(inter_hist$Round%in%c("Quarter-finals","Quarter-final"),3,
                                          ifelse(inter_hist$Round%in%c("Round of 16","First knockout round"),2,1))))


url <- "https://en.wikipedia.org/wiki/A.C._Milan_in_European_football"
milan_eu_hist <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[5]') %>%
  html_table(fill = T) %>%
  as.data.frame() %>% 
  filter(Competition=="Champions League")
milan_eu_hist<-milan_eu_hist[20:nrow(milan_eu_hist), ]
milan_eu_hist$Season <- factor(milan_eu_hist$Season) 
milan_hist<-milan_eu_hist %>% group_by(Season) %>% slice(.,n()) %>% select(Season,Round,Opposition)
milan_hist$points <- ifelse(milan_hist$Round%in%c("Final"),5,
                            ifelse(milan_hist$Round%in%c("Semi-finals","Semifinals","Semi-final"),4,
                                   ifelse(milan_hist$Round%in%c("Quarter-finals","Quarter-final"),3,
                                          ifelse(milan_hist$Round%in%c("Round of 16","First knockout round"),2,1))))


mean(juve_hist$points)
mean(milan_hist$points)
mean(inter_hist$points)

juve_hist2 <- juve_hist %>% select(Season,points) %>% mutate(team="Juve")
inter_hist2 <- inter_hist %>% select(Season,points) %>% mutate(team="Inter")
milan_hist2 <- milan_hist %>% select(Season,points) %>% mutate(team="Milan")

comp <- rbind(juve_hist2,inter_hist2,milan_hist2)
comp$Season <- factor(comp$Season)

comp<-comp %>% group_by(team) %>% mutate(trail_points=zoo::rollmeanr(points,3,fill=NA))


ggplot(comp, aes(x=Season,y=points, group=team, colour=team)) + geom_point() + geom_path(inherit.aes = T)



## Full Juve History ---


fj <- full_juve[40:nrow(full_juve), ]
fj <- fj %>% select(-Reference)
fj$Season <- as.factor(fj$Season)

  

## Example of grabbing results for 1996
  
library(data.table)
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


ggplot(UCL_Juve, aes(x=match_num,y=Form)) + 
  geom_line() +
  geom_point(data=UCL_Juve, aes(x=match_num, y=points, color=Match_Outcome)) + 
  scale_color_manual(values = c("#bdbdbd","#e34a33","#31a354")) +
  facet_wrap(vars(year),ncol = 2) + 
  theme_minimal() + xlab("Match Number") + ylab("Form - % of Points taken from Last 3 Matches") + 
  ggtitle("Juventus UCL Record") +
  theme(axis.text.y = element_blank(), legend.position = "bottom")


post_2013<-UCL_Juve[68:nrow(UCL_Juve), ]

ggplot(post_2013, aes(x=match_num,y=Form)) + 
  geom_line() +
  geom_point(data=post_2013, aes(x=match_num, y=points, color=Match_Outcome)) + 
  scale_color_manual(values = c("#bdbdbd","#e34a33","#31a354")) +
  geom_vline(xintercept = 0.5,linetype=3) + # group stage
  geom_vline(xintercept = 6.5,linetype=3) + # round of 16
  geom_vline(xintercept = 8.5,linetype=3) + # qtr finals
  geom_vline(xintercept = 10.5,linetype=3) + # semi final
  geom_vline(xintercept = 11.5,linetype=3) + # final line
  facet_wrap(vars(year),ncol = 2) + 
  theme_minimal() + xlab("Match Number") + ylab("Form - % of Points taken from Last 3 Matches") + 
  ggtitle("Juventus UCL Record") +
  theme(axis.text.y = element_blank(), legend.position = "bottom")


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





