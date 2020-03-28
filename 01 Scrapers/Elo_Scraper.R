# This script gathers historical ELO ratings produced by the website http://clubelo.com/API.
# Code is included for Italy, England, and Spain



library(dplyr)



# Downloading one file to get all team names to inform URLS to scrape
# Same process is followed below for Spain and England

download.file(url="http://api.clubelo.com/2020-01-01","elo_master.csv")

# Reading in all team names
elo<-read.csv("elo_master.csv",stringsAsFactors = F)



# Italy

    italy_teams <- elo %>% filter(Country=="ITA") %>% select(Club) %>% unique() %>% as.vector()

    # Downloading ELO history for every team in italy_teams
    for (i in 1:nrow(italy_teams)){

      # random sleep timer
      slp<-sample(c(2,3,4,5),1)
      Sys.sleep(slp)

      Team<-italy_teams$Club[i]
      url_path <- paste("http://api.clubelo.com/",Team,sep="")

      csv_title<-paste(Team,"Italy",Sys.Date(),".csv",sep="_")

      download.file(url=url_path,csv_title)

    }

    # Reading in all ELO History CSVs filtered to only include records from 2012 onwards
    files <- list.files(pattern = "\\may19.csv$")

    imported_csv <- list()
    for (i in files){
      tmp <-read.csv(i,stringsAsFactors = F)
      tmp$To <- as.Date(tmp$To,format = "%Y-%m-%d")
      tmp$From <- as.Date(tmp$From,format = "%Y-%m-%d")
      tmp$From_yr <- lubridate::year(tmp$From)

      tmp <- tmp %>% filter(From_yr>2012)
      imported_csv[[i]] <- tmp
    }

    italy_elos<-bind_rows(imported_csv)
    italy_elos <- italy_elos %>% select(-Rank,-Country,-Level,-From_yr)

    save(italy_elos,file="italy_elos.rdata")





## Spain

    setwd("/home/ravisolter/Personal_Git/Other Scrapers/England_Elo/")

    esp_teams <- elo %>% filter(Country=="ESP") %>% select(Club) %>% unique() %>% as.vector()

    # Downloading ELO history for every team in esp_teams
    for (i in 1:nrow(esp_teams)){

      slp<-sample(c(2,3,4,5),1)
      Sys.sleep(slp)

      Team<-esp_teams$Club[i]
      url_path <- paste("http://api.clubelo.com/",Team,sep="")

      csv_title<-paste(Team,"ES_may19.csv",sep="_")

      download.file(url=url_path,csv_title)

    }

    # Reading in all ELO History CSVs filtered to only include records from 2012 onwards
    files <- list.files(pattern = "\\may19.csv$")

    imported_csv <- list()
    for (i in files){
      tmp <-read.csv(i,stringsAsFactors = F)
      tmp$To <- as.Date(tmp$To,format = "%Y-%m-%d")
      tmp$From <- as.Date(tmp$From,format = "%Y-%m-%d")
      tmp$From_yr <- lubridate::year(tmp$From)

      tmp <- tmp %>% filter(From_yr>2012)
      imported_csv[[i]] <- tmp
    }

    spain_elos<-bind_rows(imported_csv)
    spain_elos <- spain_elos %>% select(-Rank,-Country,-Level,-From_yr)


## England

    eng_teams <- elo %>% filter(Country=="ENG") %>% select(Club) %>% unique() %>% as.vector()

    for (i in 1:nrow(eng_teams)){

      slp<-sample(c(2,3,4,5),1)
      Sys.sleep(slp)

      Team<-eng_teams$Club[i]
      url_path <- paste("http://api.clubelo.com/",Team,sep="")

      csv_title<-paste(Team,"ENG_may19.csv",sep="_")

      download.file(url=url_path,csv_title)

    }
