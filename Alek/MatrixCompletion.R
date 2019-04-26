####################################################
#
# This script calculates game data based on matrix completion.
# It also adds some variables to the df such as result, and 
# average form
#
###################################################

library(dplyr)
library(softImpute)

##Take archive data
archive_data <- read.csv("SerieAArchive.csv")
archive_data <- archive_data[,c("Team_h","Team_a","goals_h","goals_a","saves_h","saves_a","pen_h","pen_a","shots_h","shots_a",
                  "shots_on_h","shots_on_a","shot_on_fk_h","shot_on_fk_a","shots_off_h","shots_off_a",
                  "shot_off_fk_h","shot_off_fk_a","shots_box_h","shots_box_a","shots_sp_on_h","shots_sp_on_a","fouls_h","fouls_a",
                  "scoring_chances_h","scoring_chances_a","offsides_h","offsides_a","corners_h","corners_a"
                  ,"balls_lost_h","balls_lost_a","balls_won_h","balls_won_a","shots_sp_off_h",
                  "shots_sp_off_a")]

#Make sure it's numeric
for(i in 3:ncol(archive_data)){
  archive_data[,i] <- as.numeric(archive_data[,i])
}

#Split into each season
s2015_16 <- archive_data[1:380,]
s2016_17 <- archive_data[381:760,]
s2017_18 <- archive_data[761:1140,]

#Copy season data for use in the completion script
comp15 <- s2015_16
comp16 <- s2016_17
comp17 <- s2017_18

#Loop through rounds 6:37
for(i in 6:37){
  
  #Create temporary df's of data being completed (by round)
  temp15 <- s2015_16[(10*i+1):(10*i + 10),]
  temp15[,3:ncol(archive_data)] <- NA
  
  temp16 <- s2016_17[(10*i+1):(10*i + 10),]
  temp16[,3:ncol(archive_data)] <- NA
  
  temp17 <- s2017_18[(10*i+1):(10*i + 10),]
  temp17[,3:ncol(archive_data)] <- NA
  
  #Go through each game in a round
  for(j in 1:10){
    
    #Find the home/away games previously played by clubs (this loops through each season, so I'll only explain it once)
    home15 <- filter(s2015_16[1:(10*i),], Team_h == temp15[j,1])
    away15 <- filter(s2015_16[1:(10*i),], Team_a == temp15[j,2])
    
    #To control for form, I only use the last 5 results
    if(nrow(home15) > 5){
      home15 <- home15[(nrow(home15)-4):nrow(home15),]
    }
    if(nrow(away15) > 5){
      away15 <- away15[(nrow(away15)-4):nrow(away15),]
    }
    
    #Create matrix of home/away data, and the uncomplete game
    temp3 <- rbind(home15, away15)
    temp3 <- rbind(temp3,temp15[j,])
    temp3 <- temp3[,3:ncol(archive_data)]
    temp3 <- as.matrix(temp3)
    
    home16 <- filter(s2016_17[1:(10*i),], Team_h == temp16[j,1])
    away16 <- filter(s2016_17[1:(10*i),], Team_a == temp16[j,2])
    if(nrow(home16) > 5){
      home16 <- home16[(nrow(home16)-4):nrow(home16),]
    }
    if(nrow(away16) > 5){
      away16 <- away16[(nrow(away16)-4):nrow(away16),]
    }
    temp4 <- rbind(home16, away16)
    temp4 <- rbind(temp4,temp16[j,])
    temp4 <- temp4[,3:ncol(archive_data)]
    temp4 <- as.matrix(temp4)
    
    home17 <- filter(s2017_18[1:(10*i),], Team_h == temp17[j,1])
    away17 <- filter(s2017_18[1:(10*i),], Team_a == temp17[j,2])
    if(nrow(home17) > 5){
      home17 <- home17[(nrow(home17)-4):nrow(home17),]
    }
    if(nrow(away17) > 5){
      away17 <- away17[(nrow(away17)-4):nrow(away17),]
    }
    temp5 <- rbind(home17, away17)
    temp5 <- rbind(temp5,temp17[j,])
    temp5 <- temp5[,3:ncol(archive_data)]
    temp5 <- as.matrix(temp5)

    if (ceiling(nrow(temp3)/2) < min(dim(temp3)) - 1){
      rank <- ceiling(nrow(temp3)/2)
    } else {
      rank <- min(dim(temp3)) - 1
    }
    
    #Run the matrix completion, and set aside the completed game data
    fit <- softImpute(temp3,rank.max = rank,lambda = 0)
    completetemp <- data.frame(complete(temp3,fit))
    temp15[j,3:ncol(archive_data)] <- completetemp[nrow(completetemp),]
    
    fit <- softImpute(temp4,rank.max = rank,lambda = 0)
    completetemp <- data.frame(complete(temp4,fit))
    temp16[j,3:ncol(archive_data)] <- completetemp[nrow(completetemp),]
    
    fit <- softImpute(temp5,rank.max = rank,lambda = 0)
    completetemp <- data.frame(complete(temp5,fit))
    temp17[j,3:ncol(archive_data)] <- completetemp[nrow(completetemp),]
    
  }
  
  #Replace the doublicate data from earlier (row 32:34) with the completed data
  comp15[(10*i+1):(10*i + 10),3:ncol(archive_data)] <- temp15[,3:ncol(archive_data)]
  comp16[(10*i+1):(10*i + 10),3:ncol(archive_data)] <- temp16[,3:ncol(archive_data)]
  comp17[(10*i+1):(10*i + 10),3:ncol(archive_data)] <- temp17[,3:ncol(archive_data)]
}

#Add real goals scored
comp15$real_home <- s2015_16$goals_h
comp15$real_away <- s2015_16$goals_a
comp16$real_home <- s2016_17$goals_h
comp16$real_away <- s2016_17$goals_a
comp17$real_home <- s2017_18$goals_h
comp17$real_away <- s2017_18$goals_a

#Put all the seasons together
comp <- rbind(comp15[61:380,],comp15[61:380,])
comp <- rbind(comp,comp17[61:380,])

#################################
#
# This section just runs through the current season data.
# It works exactly like the stuff above, it just has a different number of
# loops so it's seperate
#
#################################

data <- read.csv("SerieA201819.csv")
data <- data[,c("Team_h","Team_a","goals_h","goals_a","saves_h","saves_a","pen_h","pen_a","shots_h","shots_a",
                "shots_on_h","shots_on_a","shot_on_fk_h","shot_on_fk_a","shots_off_h","shots_off_a",
                "shot_off_fk_h","shot_off_fk_a","shots_box_h","shots_box_a","shots_sp_on_h","shots_sp_on_a","fouls_h","fouls_a",
                "scoring_chances_h","scoring_chances_a","offsides_h","offsides_a","corners_h","corners_a"
                ,"balls_lost_h","balls_lost_a","balls_won_h","balls_won_a","shots_sp_off_h",
                "shots_sp_off_a")]
temp <- data

for (i in 6:(nrow(data)%/%10)-1){
  
  temp2 <- data[(10*i+1):(10*i + 10),]
  temp2[,3:ncol(archive_data)] <- NA
  
  for(j in 1:10){
    home <- filter(data[1:(10*i),], Team_h == temp2[j,1])
    away <- filter(data[1:(10*i),], Team_a == temp2[j,2])
    if(nrow(home) > 5){
      home <- home[(nrow(home)-4):nrow(home),]
    }
    if(nrow(away) > 5){
      away <- away[(nrow(away)-4):nrow(away),]
    }
    temp3 <- rbind(home, away)
    temp3 <- rbind(temp3,temp2[j,])
    temp3 <- temp3[,3:ncol(archive_data)]
    temp3 <- as.matrix(temp3)
    
    if (ceiling(nrow(temp3)/2) < min(dim(temp3)) - 1){
      rank <- ceiling(nrow(temp3)/2)
    } else {
      rank <- min(dim(temp3)) - 1
    }
    
    fit <- softImpute(temp3,rank.max = rank,lambda = 0)
    completetemp <- data.frame(complete(temp3,fit))
    
    temp2[j,3:ncol(archive_data)] <- completetemp[nrow(completetemp),]
  }
  temp[(10*i+1):(10*i + 10),3:ncol(archive_data)] <- temp2[,3:ncol(archive_data)]
}

test <- temp[61:nrow(temp),]
test$real_home <- data[61:nrow(temp),3]
test$real_away <- data[61:nrow(temp),4]

comp <- rbind(comp,test)

#################################################

# Create a result variable
# 1 = home win, 2 = draw, 3 = away win

archive_data <- rbind(archive_data,data)
archive_data$result <- NA
for (i in 1:nrow(archive_data)){
  if (archive_data[i,3] > archive_data[i,4]){
    archive_data[i,37] <- 1
  }
  if (archive_data[i,3] == archive_data[i,4]){
    archive_data[i,37] <- 2
  }
  if (archive_data[i,3] < archive_data[i,4]){
    archive_data[i,37] <- 3
  }
}

# Calculate a 5 game points running average for the home/away teams
archive_data$home_points_av <- NA
archive_data$away_points_av <- NA

for (i in c(61:380,441:760,821:1140,1201:nrow(archive_data))){
  temp <- archive_data[1:(i-1),]
  
  temp2 <- filter(temp, temp$Team_h == archive_data[i,1])
  if (nrow(temp2) > 5){
    temp2 <- temp2[(nrow(temp2)-4):nrow(temp2),]
  }
  a <- c()
  for(j in 1:nrow(temp2)){
    points <- 0
    
    if(temp2[j,37] == 1){
      points <- 3
    }
    if(temp2[j,37] == 2){
      points <- 1
    }
    a[j] <- points
  }
  archive_data[i,38] <- mean(a)
  
  temp2 <- filter(temp, temp$Team_a == archive_data[i,2])
  if (nrow(temp2) > 5){
    temp2 <- temp2[(nrow(temp2)-4):nrow(temp2),]
  }
  a <- c()
  for(j in 1:nrow(temp2)){
    points <- 0
    
    if(temp2[j,37] == 3){
      points <- 3
    }
    if(temp2[j,37] == 2){
      points <- 1
    }
    a[j] <- points
  }
  archive_data[i,39] <- mean(a)
  
}

#Adds result, and averages to the completed data
comp[,39:41] <- archive_data[c(61:380,441:760,821:1140,1201:nrow(archive_data)),37:39]

rm(away)
rm(away15)
rm(away16)
rm(away17)
rm(completetemp)
rm(fit)
rm(home)
rm(home15)
rm(home16)
rm(home17)
rm(rank)
rm(data)
rm(archive_data)
rm(test)
rm(comp15)
rm(comp16)
rm(comp17)
rm(temp15)
rm(temp16)
rm(temp17)
rm(temp)
rm(temp2)
rm(temp3)
rm(temp4)
rm(temp5)
rm(s2015_16)
rm(s2016_17)
rm(s2017_18)
