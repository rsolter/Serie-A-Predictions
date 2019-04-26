library(tidyverse)



### adding variables to understand avg. return on uniform betting

odds$match_id <- nrow(odds):1
odds <- odds %>% arrange(match_id)

rounds <- list()
for(i in 1:25) rounds[[i]] <- print(rep(i,10))
r<-unlist(rounds)

odds$round <- r[1:nrow(odds)]
rm(rounds,i,r)


### Pulling in actual results

source(file="engineering_seriea.R")

DFr <- DF %>% filter(season=="2018-19") %>% select(`Home Team`,`Away Team`,result)

odds$Home<- str_trim(odds$Home, side = c("both"))
odds$Away<- str_trim(odds$Away, side = c("both"))

odds$Home <- ifelse(odds$Home=="Chievo","Chievoverona",
                    ifelse(odds$Home=="AS Roma","Roma",
                    ifelse(odds$Home=="AC Milan","Milan",odds$Home)))

odds$Away <- ifelse(odds$Away=="Chievo","Chievoverona",
                    ifelse(odds$Away=="AS Roma","Roma",
                    ifelse(odds$Away=="AC Milan","Milan",odds$Away)))


# matches all but last round (25) since we're missing results for taht
odds <- left_join(odds,DFr, by=c("Home"="Home Team",
                                 "Away"="Away Team"))

odds <- odds %>% filter(round<25)
rm(DFr,DF)


#### Adding in successful return and unsuccssfull cost

odds$unsuccessful_cost <- -10

# calculating the different returs
odds$successful_rev <- 
# returns on away wins
    ifelse(odds$result=="Away"&odds$`2`<0,(10*(odds$`2`-100)/odds$`2`),
    ifelse(odds$result=="Away"&odds$`2`>0,(10*odds$`2`/100),
# returns on home wins
    ifelse(odds$result=="Home"&odds$`1`<0,(10*(odds$`1`-100)/odds$`1`),
    ifelse(odds$result=="Home"&odds$`1`>0,(10*odds$`1`/100),
# returns on Draws -- interestingly, draws are never favorites in this data
    ifelse(odds$result=="Draw"&odds$`X`<0,(10*(odds$X-100)/odds$X),
    ifelse(odds$result=="Draw"&odds$`X`>0,(10*odds$X/100),
           NA))))))


odds$successfuL_profit <- odds$successful_rev - 10


# Subsetting odds into rounds just rounds, game_ids and returns 
s_odds <- odds %>% select(Match,result,round,unsuccessful_cost,successful_rev)


# Test with round 1

test <- s_odds %>% filter(round==1)

test_output<-list()
for (i in 3:8){
  max <- top_n(test,i,successful_rev)
  max_profit  <- sum(max$successful_rev) - 10*(10-i)
  
  min <- top_n(test,-i,successful_rev)
  min_profit <- sum(min$successful_rev) - 10*(10-i)
  
  output <- data.frame(i, min_profit, max_profit)
  test_output[[i]] <- output
}

output <- bind_rows(test_output[3:8])



# Loop for all the rounds

test_output <- list()
for(j in 1:max(odds$round)){
  test <- s_odds %>% filter(round==j)
  
  round_output<-list()
  for (i in 3:8){
    max <- top_n(test,i,successful_rev)
    max_profit  <- sum(max$successful_rev) - 10*(10-i)
    
    min <- top_n(test,-i,successful_rev)
    min_profit <- sum(min$successful_rev) - 10*(10-i)
    
    output <- data.frame(i, min_profit, max_profit)
    round_output[[i]] <- output
  }
  
  output <- bind_rows(round_output[3:8])
  test_output[[j]] <- output
}


final <- bind_rows(test_output)
save(final,file = "final_output.rdata")


gathered_final <- gather(final,"Correct","Profit",-1)
#gathered_final <- gathered_final %>% select(-Correct)

gathered_final$i <- factor(gathered_final$i) 
library(ggplot2)

ggplot(gathered_final, aes(x=i,y=Profit)) + 
  geom_jitter() + 
  theme_minimal() + 
  geom_hline(yintercept = 0)


final$i <- factor(final$i) 
ggplot(final, aes(x=i,y=min_profit)) + 
  geom_jitter() + geom_boxplot() +
  theme_minimal() + 
  geom_hline(yintercept = 0) +
  xlab("# Correct Predictions") + 
  ylab("Profit") + scale_y_continuous(labels = scales::dollar)









