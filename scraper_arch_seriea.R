# This script grabs historical season data for Serie A matches

library(xml2)
library(rvest)
library(stringr)


data <- data.frame(matrix(NA, nrow = 0, ncol = 48))
names <- c("Team_h","Team_a","goals_h","goals_a","saves_h","saves_a","pen_h","pen_a","shots_h","shots_a",
           "shots_on_h","shots_on_a","shot_on_fk_h","shot_on_fk_a","shots_off_h","shots_off_a",
           "shot_off_fk_h","shot_off_fk_a","shots_box_h","shots_box_a","shots_sp_on_h","shots_sp_on_a","fouls_h","fouls_a",
           "scoring_chances_h","scoring_chances_a","offsides_h","offsides_a","corners_h","corners_a"
           ,"yellow_h","yellow_a","red_h","red_a","balls_lost_h","balls_lost_a","balls_won_h","balls_won_a","shots_sp_off_h",
           "shots_sp_off_a",
           "attacks_middle_h","attacks_middle_a","attacks_right_h","attacks_right_a","attacks_left_h","attacks_left_a","season","round")


# Selecting years from the archive to scrape
arch <- c("2015-16","2016-17","2017-18")

## 2016-2017, Round 2 throws an error with a foreited between Sas and Pescara 
## http://www.legaseriea.it/en/serie-a/archive/2016-17/UNICO/UNI/2


for (k in 1:length(arch)){
roundurl_stem <<- paste("http://www.legaseriea.it/en/serie-a/archive/",arch[[k]],"/UNICO/UNI/",sep="")
print(arch[k])
#print(roundurl_stem)

for(i in 1:38){
  Sys.sleep(4)
  roundurl <- paste(roundurl_stem,i,sep="")
  x <- read_html(roundurl)
  
  #print(roundurl)
  
  for(j in 1:10){
    a <- c()
    gamenode <- paste("/html/body/main/div[1]/section[1]/section/div[",j,sep="")
    gamenode <- paste(gamenode,"]/div[4]/a[2]",sep="")
    
    if ((k == 2) & (i == 2) & (j == 8)){
      gamenode <- paste("/html/body/main/div[1]/section[1]/section/div[",j,sep="")
      gamenode <- paste(gamenode,"]/div[5]/a[2]",sep="")      
    }
    if ((k ==2) & (i ==18) & ((j==9)|(j==10))){
      gamenode <- paste("/html/body/main/div[1]/section[1]/section/div[",j,sep="")
      gamenode <- paste(gamenode,"]/div[4]/a",sep="")        
    }
    
    match <- html_node(x, xpath=gamenode)
    match <- as.character(html_attrs(match))
    match <- paste("http://www.legaseriea.it",match,sep="")
    
    Sys.sleep(4)
    report <- read_html(match)
    
    team <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[1]')
    team <- html_text(team)
    team <- str_replace_all(team, "[\r\n ]" , "")
    team <- str_replace(team, "Matchstatistics",",")
    team <- unlist(strsplit(team, ","))
    
    a[1] <- team[1]
    a[2] <- team[2]
    
    goals_h <- html_node(report,xpath='/html/body/main/div[1]/section/div[1]/div[3]')
    goals_h <- html_text(goals_h)
    a[3] <- goals_h
    goals_a <- html_node(report,xpath='/html/body/main/div[1]/section/div[1]/div[5]')
    goals_a <- html_text(goals_a)
    a[4] <- goals_a
    
    saves_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[3]/div[2]')
    saves_h <- html_text(saves_h)
    a[5] <- saves_h
    saves_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[3]/div[4]')
    saves_a <- html_text(saves_a)
    a[6] <- saves_a
    
    pen_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[4]/div[2]')
    pen_h <- html_text(pen_h)
    a[7] <- pen_h
    pen_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[4]/div[4]')
    pen_a <- html_text(pen_a)
    a[8] <- pen_a
    
    shot_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[5]/div[2]')
    shot_h <- html_text(shot_h)
    a[9] <- shot_h
    shot_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[5]/div[4]')
    shot_a <- html_text(shot_a)
    a[10] <- shot_a
    
    shot_o_t_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[6]/div[2]')
    shot_o_t_h <- html_text(shot_o_t_h)
    a[11] <- shot_o_t_h
    shot_o_t_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[6]/div[4]')
    shot_o_t_a <- html_text(shot_o_t_a)
    a[12] <- shot_o_t_a
    
    shot_o_t_fk_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[7]/div[2]')
    shot_o_t_fk_h <- html_text(shot_o_t_fk_h)
    a[13] <- shot_o_t_fk_h
    shot_o_t_fk_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[7]/div[4]')
    shot_o_t_fk_a <- html_text(shot_o_t_fk_a)
    a[14] <- shot_o_t_fk_a
    
    off_shot_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[8]/div[2]')
    off_shot_h <- html_text(off_shot_h)
    a[15] <- off_shot_h
    off_shot_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[8]/div[4]')
    off_shot_a <- html_text(off_shot_a)
    a[16] <- off_shot_a
    
    off_shot_fk_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[9]/div[2]')
    off_shot_fk_h <- html_text(off_shot_fk_h)
    a[17] <- off_shot_fk_h
    off_shot_fk_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[9]/div[4]')
    off_shot_fk_a <- html_text(off_shot_fk_a)
    a[18] <- off_shot_fk_a
    
    box_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[10]/div[2]')
    box_h <- html_text(box_h)
    a[19] <- box_h
    box_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[10]/div[4]')
    box_a <- html_text(box_a)
    a[20] <- box_a
    
    set_o_t_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[11]/div[2]')
    set_o_t_h <- html_text(set_o_t_h)
    a[21] <- set_o_t_h
    set_o_t_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[11]/div[4]')
    set_o_t_a <- html_text(set_o_t_a)
    a[22] <- set_o_t_a
    
    fouls_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[13]/div[2]')
    fouls_h <- html_text(fouls_h)
    a[23] <- fouls_h
    fouls_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[13]/div[4]')
    fouls_a <- html_text(fouls_a)
    a[24] <- fouls_a
    
    chances_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[16]/div[2]')
    chances_h <- html_text(chances_h)
    a[25] <- chances_h
    chances_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[16]/div[4]')
    chances_a <- html_text(chances_a)
    a[26] <- chances_a
    if (k ==1 | k == 2 | k ==3){
      chances_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[15]/div[2]')
      chances_h <- html_text(chances_h)
      a[25] <- chances_h
      chances_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[15]/div[4]')
      chances_a <- html_text(chances_a)
      a[26] <- chances_a
    }
    
    off_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[18]/div[2]')
    off_h <- html_text(off_h)
    a[27] <- off_h
    off_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[18]/div[4]')
    off_a <- html_text(off_a)
    a[28] <- off_a
    a[26] <- chances_a
    if (k ==1 | k == 2 | k == 3){
      off_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[17]/div[2]')
      off_h <- html_text(off_h)
      a[27] <- off_h
      off_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[17]/div[4]')
      off_a <- html_text(off_a)
      a[28] <- off_a
    }
    
    cor_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[19]/div[2]')
    cor_h <- html_text(cor_h)
    a[29] <- cor_h
    cor_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[19]/div[4]')
    cor_a <- html_text(cor_a)
    a[30] <- cor_a
    if (k ==1 | k == 2 | k ==3){
      cor_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[18]/div[2]')
      cor_h <- html_text(cor_h)
      a[29] <- cor_h
      cor_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[18]/div[4]')
      cor_a <- html_text(cor_a)
      a[30] <- cor_a
    }

    yell_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[22]/div[2]')
    yell_h <- html_text(yell_h)
    a[31] <- yell_h
    yell_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[22]/div[4]')
    yell_a <- html_text(yell_a)
    a[32] <- yell_a
    if(k ==1 | k == 2 | k == 3){
      yell_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[19]/div[2]')
      yell_h <- html_text(yell_h)
      a[31] <- yell_h
      yell_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[19]/div[4]')
      yell_a <- html_text(yell_a)
      a[32] <- yell_a
    }
    
    red_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[24]/div[2]')
    red_h <- html_text(red_h)
    a[33] <- red_h
    red_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[24]/div[4]')
    red_a <- html_text(red_a)
    a[34] <- red_a
    if(k ==1 | k == 2 | k == 3){
      red_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[21]/div[2]')
      red_h <- html_text(red_h)
      a[33] <- red_h
      red_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[21]/div[4]')
      red_a <- html_text(red_a)
      a[34] <- red_a
    }
    
    lost_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[25]/div[2]')
    lost_h <- html_text(lost_h)
    a[35] <- lost_h
    lost_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[25]/div[4]')
    lost_a <- html_text(lost_a)
    a[36] <- lost_a
    if(k == 1 | k == 2 | k == 3){
      lost_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[22]/div[2]')
      lost_h <- html_text(lost_h)
      a[35] <- lost_h
      lost_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[22]/div[4]')
      lost_a <- html_text(lost_a)
      a[36] <- lost_a
    }
    
    won_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[26]/div[2]')
    won_h <- html_text(won_h)
    a[37] <- won_h
    won_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[26]/div[4]')
    won_a <- html_text(won_a)
    a[38] <- won_a
    if(k == 1 | k == 2 | k == 3){
      won_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[23]/div[2]')
      won_h <- html_text(won_h)
      a[37] <- won_h
      won_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[23]/div[4]')
      won_a <- html_text(won_a)
      a[38] <- won_a
    }
    
    shots_sp_off_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[12]/div[2]')
    shots_sp_off_h <- html_text(shots_sp_off_h)
    a[39] <- shots_sp_off_h
    shots_sp_off_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[12]/div[4]')
    shots_sp_off_a <- html_text(shots_sp_off_a)
    a[40] <- shots_sp_off_a
    
    # fast_breaks_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[24]/div[2]')
    # fast_breaks_h <- html_text(fast_breaks_h)
    # a[41] <- fast_breaks_h
    # fast_breaks_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[24]/div[4]')
    # fast_breaks_a <- html_text(fast_breaks_a)
    # a[42] <- fast_breaks_a
    # 
    # crosses_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[25]/div[2]')
    # crosses_h <- html_text(crosses_h)
    # a[43] <- crosses_h
    # crosses_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[25]/div[4]')
    # crosses_a <- html_text(crosses_a)
    # a[44] <- crosses_a
    # 
    # long_balls_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[26]/div[2]')
    # long_balls_h <- html_text(long_balls_h)
    # a[45] <- long_balls_h
    # long_balls_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[26]/div[4]')
    # long_balls_a <- html_text(long_balls_a)
    # a[46] <- long_balls_a
    # 
    attacks_middle_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[27]/div[2]')
    attacks_middle_h <- html_text(attacks_middle_h)
    a[41] <- attacks_middle_h
    attacks_middle_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[27]/div[4]')
    attacks_middle_a <- html_text(attacks_middle_a)
    a[42] <- attacks_middle_a
    
    attacks_right_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[28]/div[2]')
    attacks_right_h <- html_text(attacks_right_h)
    a[43] <- attacks_right_h
    attacks_right_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[28]/div[4]')
    attacks_right_a <- html_text(attacks_right_a)
    a[44] <- attacks_right_a
    
    attacks_left_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[28]/div[2]')
    attacks_left_h <- html_text(attacks_left_h)
    a[45] <- attacks_left_h
    attacks_left_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[28]/div[4]')
    attacks_left_a <- html_text(attacks_left_a)
    a[46] <- attacks_left_a
    
    a[47] <- arch[[k]]
    
    a[48] <- i
    
    data[(nrow(data)+1),] <- a
    }
  
  }
}

# correcting for the match that was forfeited.
data[398,3:4] <- c(2,1)

colnames(data) <- names
for(i in c(1:2,47:48)){
  data[,i] <- as.factor(data[,i])
}
for(i in 3:46){
  data[,i] <- as.numeric(data[,i])
}

archive<-data

#write.csv(archive,"SerieAArchive.csv",row.names = F)

save(archive,file="archive_serie.rdata")


