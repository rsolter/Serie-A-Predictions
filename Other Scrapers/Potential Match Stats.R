library(rvest)
library(tidyverse)
library(stringr)
library(profvis)


#http://www.soccerstats247.com/robots.txt
# has results for matches


# Grabbing one matches stats in a table format
url <- "http://www.soccerstats247.com/matches/italy/internazionale-juventus-2418404/"
url <- "http://www.soccerstats247.com/matches/italy/genoa-lazio-2503361/"

# pulling down table
temp_hist <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="soccerContentPlaceHolder_panelStatistics"]/table') %>%
  html_table(fill = T) %>%
  as.data.frame() 


# Looking for leagues being played on a certain day
url2 <- "http://www.soccerstats247.com/matches/2019-2-17/"

temp_hist2 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="soccerContentPlaceHolder_divMatches"]') %>%
  html_text()


