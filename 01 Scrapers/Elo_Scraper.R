# This script gathers historical ELO ratings produced by the website http://clubelo.com/API.
# Code is included for Italy and Spain



library(dplyr)



# Downloading one file to get all team names to inform URLS to scrape
# Same process is followed below for Spain and England

download.file(url="http://api.clubelo.com/2019-01-01","01 Scrapers/elo_master.csv")

# Reading in all team names
elo<-read.csv("01 Scrapers/elo_master.csv",stringsAsFactors = F)



# Italy

    italy_teams <- elo %>% filter(Country=="ITA") %>% select(Club) %>% unique() %>% as.vector()
    
    italy_teams <- italy_teams$Club
    
    italy_teams <- c(italy_teams,"Palermo","Carpi","Chievo") # note that Verona is Hellas Verona

    # Downloading ELO history for every team in italy_teams
    for (i in 1:length(italy_teams)){

      # random sleep timer
      slp<-sample(c(2,3,4,5),1)
      Sys.sleep(slp)

      Team<-italy_teams[i]
      url_path <- paste("http://api.clubelo.com/",Team,sep="")

      csv_title<-paste("01 Scrapers/Italy/",Team,"Italy",Sys.Date(),".csv",sep="_")

      download.file(url=url_path,csv_title)

    }

    # Reading in all ELO History CSVs filtered to only include records from 2012 onwards
    files <- list.files(path = "01 Scrapers/Italy/",pattern = "\\.csv$")

    imported_csv <- list()
    for (i in files){
      tmp <-read.csv(paste("01 Scrapers/Italy/",i,sep=""),stringsAsFactors = F)
      tmp$To <- as.Date(tmp$To,format = "%Y-%m-%d")
      tmp$From <- as.Date(tmp$From,format = "%Y-%m-%d")
      tmp$From_yr <- lubridate::year(tmp$From)

      tmp <- tmp %>% filter(From_yr>2012)
      imported_csv[[i]] <- tmp
    }

    italy_elos<-bind_rows(imported_csv) %>% unique()
    italy_elos <- italy_elos %>% select(-Rank,-Country,-Level,-From_yr)
    
    
    # Renaming two clubs to match other dataset
    italy_elos$Club <- ifelse(italy_elos$Club=="Verona","HellasVerona",
                              ifelse(italy_elos$Club=="Chievo","Chievoverona",
                                     italy_elos$Club))
    
    italy_elos$Club <- as.factor(italy_elos$Club)
    
    save(italy_elos,file="00 Data/italy_elos.rdata")
    





## Spain

    esp_teams <- elo %>% filter(Country=="ESP") %>% select(Club) %>% unique() %>% as.vector()

    # Downloading ELO history for every team in esp_teams
    for (i in 1:nrow(esp_teams)){

      slp<-sample(c(2,3,4,5),1)
      Sys.sleep(slp)

      Team<-esp_teams$Club[i]
      url_path <- paste("http://api.clubelo.com/",Team,sep="")

      csv_title<-paste("01 Scrapers/Spain/",Team,"Spain",Sys.Date(),".csv",sep="_")

      download.file(url=url_path,csv_title)

    }

    # Reading in all ELO History CSVs filtered to only include records from 2012 onwards
    files <- list.files(path = "01 Scrapers/Spain/",pattern = "\\.csv$")

    imported_csv <- list()
    for (i in files){
      tmp <-read.csv(paste("01 Scrapers/Spain/",i,sep=""),stringsAsFactors = F)
      tmp$To <- as.Date(tmp$To,format = "%Y-%m-%d")
      tmp$From <- as.Date(tmp$From,format = "%Y-%m-%d")
      tmp$From_yr <- lubridate::year(tmp$From)

      tmp <- tmp %>% filter(From_yr>2012)
      imported_csv[[i]] <- tmp
    }

    spain_elos<-bind_rows(imported_csv)
    spain_elos <- spain_elos %>% select(-Rank,-Country,-Level,-From_yr)

    save(italy_elos,file="01 Scrapers/Data/Spain_elos.rdata")
