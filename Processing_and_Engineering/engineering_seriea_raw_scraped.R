#engineering raw data


library(dplyr)
library(tidyr)
library(zoo)

load(file="~/Personal_Git/Soccer_Prediction/SerieA/Data/seriea_2018_19.rdata")
load(file="~/Personal_Git/Soccer_Prediction/SerieA/Data/archive_serie.rdata")
raw_scraped <- rbind(archive,seriea_2018_19)

# Turning round into a factor
raw_scraped$round <- factor(raw_scraped$round, levels=c("1","2","3","4","5","6","7","8",
                                                        "9","10","11","12","13","14","15","16",
                                                        "17","18","19","20","21","22","23","24",
                                                        "25","26","27","28","29","30","31","32",
                                                        "33","34","35","36","37","38"))
# Adding game_id variable
raw_scraped <- raw_scraped %>% arrange(season,round)
raw_scraped$game_id <- 1:nrow(raw_scraped)


rm(archive,seriea_2018_19)
