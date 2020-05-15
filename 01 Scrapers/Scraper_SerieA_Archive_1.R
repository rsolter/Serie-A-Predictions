## Serie A Archive Scraper 1 of 2
  # For seasons '2015-16' through '2017-18'

library(xml2)
library(rvest)
library(stringr)


data <- data.frame(matrix(NA, nrow = 0, ncol = 53))
names <- c("Team_h","Team_a","goals_h","goals_a","saves_h","saves_a",
           "pen_h","pen_a","shots_h","shots_a","shots_on_h","shots_on_a","shot_on_fk_h","shot_on_fk_a",
           "shots_off_h","shots_off_a","shot_off_fk_h","shot_off_fk_a","shots_box_h","shots_box_a",
           "shots_sp_on_h","shots_sp_on_a","fouls_h","fouls_a","scoring_chances_h","scoring_chances_a",
           "offsides_h","offsides_a","corners_h","corners_a","yellow_h","yellow_a",
           "red_h","red_a","shots_sp_off_h","shots_sp_off_a",
           "fast_breaks_h","fast_breaks_a","crosses_h","crosses_a","long_balls_h","long_balls_a",
           "attacks_middle_h","attacks_middle_a",
           "attacks_right_h","attacks_right_a","attacks_left_h","attacks_left_a",
           "season","round","poss_h","poss_a","match_date")


arch <- c("2014-15")

t1<-proc.time()
for (k in 1:length(arch)){
  roundurl_stem <<- paste("http://www.legaseriea.it/en/serie-a/archive/",arch[[k]],"/UNICO/UNI/",sep="")
  print(arch[k])
  #print(roundurl_stem)
  
  for(i in 1:38){
    print(i)
    Sys.sleep(3)
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
      try(report <- read_html(match))
      
      team <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[1]')
      team <- html_text(team)
      team <- str_replace_all(team, "[\r\n ]" , "")
      team <- str_replace(team, "Matchstatistics",",")
      team <- unlist(strsplit(team, ","))
      
      a[1] <- team[1]
      a[2] <- team[2]
      
      
      ## Adding in possession
      
      poss_h <- html_node(report,xpath='/html/body/main/div[1]/section/section[4]/div[2]/div[2]')
      poss_h <- html_text(poss_h)
      a[51] <- poss_h
      
      poss_a <- html_node(report,xpath='/html/body/main/div[1]/section/section[4]/div[2]/div[4]')
      poss_a <- html_text(poss_a)
      a[52] <- poss_a
      
      match_date <- html_node(report,xpath='/html/body/main/div[1]/section/div[1]/div[1]/span')
      match_date <- html_text(match_date)
      match_date <- substr(match_date,1,10)
      a[53] <- match_date
      
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
      
      chances_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[15]/div[2]')
      chances_h <- html_text(chances_h)
      a[25] <- chances_h
      
      chances_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[15]/div[4]')
      chances_a <- html_text(chances_a)
      a[26] <- chances_a
      
      off_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[17]/div[2]')
      off_h <- html_text(off_h)
      a[27] <- off_h
      
      off_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[17]/div[4]')
      off_a <- html_text(off_a)
      a[28] <- off_a
  
      cor_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[18]/div[2]')
      cor_h <- html_text(cor_h)
      a[29] <- cor_h
      
      cor_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[18]/div[4]')
      cor_a <- html_text(cor_a)
      a[30] <- cor_a
      
      yell_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[19]/div[2]')
      yell_h <- html_text(yell_h)
      a[31] <- yell_h
      
      yell_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[19]/div[4]')
      yell_a <- html_text(yell_a)
      a[32] <- yell_a
      
      red_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[21]/div[2]')
      red_h <- html_text(red_h)
      a[33] <- red_h
      
      red_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[21]/div[4]')
      red_a <- html_text(red_a)
      a[34] <- red_a

      shots_sp_off_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[12]/div[2]')
      shots_sp_off_h <- html_text(shots_sp_off_h)
      a[35] <- shots_sp_off_h
      
      shots_sp_off_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[12]/div[4]')
      shots_sp_off_a <- html_text(shots_sp_off_a)
      a[36] <- shots_sp_off_a
      
      fast_breaks_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[22]/div[2]')
      fast_breaks_h <- html_text(fast_breaks_h)
      a[37] <- fast_breaks_h
      
      fast_breaks_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[22]/div[4]')
      fast_breaks_a <- html_text(fast_breaks_a)
      a[38] <- fast_breaks_a
       
      crosses_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[23]/div[2]')
      crosses_h <- html_text(crosses_h)
      a[39] <- crosses_h
      
      crosses_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[23]/div[4]')
      crosses_a <- html_text(crosses_a)
      a[40] <- crosses_a
       
      long_balls_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[24]/div[2]')
      long_balls_h <- html_text(long_balls_h)
      a[41] <- long_balls_h
      
      long_balls_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[24]/div[4]')
      long_balls_a <- html_text(long_balls_a)
      a[42] <- long_balls_a
      
      attacks_middle_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[25]/div[2]')
      attacks_middle_h <- html_text(attacks_middle_h)
      a[43] <- attacks_middle_h
      
      attacks_middle_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[25]/div[4]')
      attacks_middle_a <- html_text(attacks_middle_a)
      a[44] <- attacks_middle_a
      
      attacks_right_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[26]/div[2]')
      attacks_right_h <- html_text(attacks_right_h)
      a[45] <- attacks_right_h
      
      attacks_right_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[26]/div[4]')
      attacks_right_a <- html_text(attacks_right_a)
      a[46] <- attacks_right_a
      
      attacks_left_h <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[27]/div[2]')
      attacks_left_h <- html_text(attacks_left_h)
      a[47] <- attacks_left_h
      
      attacks_left_a <- html_node(report,xpath='//*[@id="statistiche-comparate"]/div[27]/div[4]')
      attacks_left_a <- html_text(attacks_left_a)
      a[48] <- attacks_left_a
      
      a[49] <- arch[[k]]
      
      a[50] <- i
      
      data[(nrow(data)+1),] <- a
      
      rm(attacks_left_a,attacks_left_h,attacks_middle_a,attacks_middle_h,attacks_right_a,attacks_right_h,
         box_a,box_h,chances_a,chances_h,cor_a,cor_h,crosses_a,crosses_h,fast_breaks_a,fast_breaks_h,fouls_a,
         fouls_h,goals_a,goals_h,long_balls_a,long_balls_h,match_date,off_a,off_h,off_shot_a,off_shot_h,off_shot_fk_a,
         off_shot_fk_h,pen_a,pen_h,poss_a,poss_h,red_a,red_h,saves_a,saves_h,set_o_t_a,set_o_t_h,shot_a,shot_h,shot_o_t_a,
         shot_o_t_h,shot_o_t_fk_a,shot_o_t_fk_h,shots_sp_off_a,shots_sp_off_h,yell_a,yell_h)
    }
    
  }
}

# correcting for the match that was forfeited.
# data[398,3:4] <- c(2,1)


t2<-proc.time()


colnames(data) <- names

for(i in c(1:2,49:50)){
  data[,i] <- as.factor(data[,i])
}
for(i in 3:48){
  data[,i] <- as.numeric(data[,i])
}
for(i in 51:52){
  data[,i] <- as.numeric(substr(data[,i],1,2))/100
}

data$match_date <- as.Date(data$match_date,format = "%d/%m/%Y")


archive <-data %>% unique()

archive$completed_passes_h <- NA
archive$completed_passes_a <- NA
archive$passing_acc_h <- NA
archive$passing_acc_a <- NA
archive$key_passes_h <- NA
archive$key_passes_a <- NA
archive$recoveries_h <- NA
archive$recoveries_a <- NA 

save(archive,file="00 Data/archive_seriea_1516_1718.rdata")

