
#### Set Up ----
library(randomForest)
library(tidyverse)
library(kableExtra)
library(caret)
library(caretEnsemble)
library(e1071)
library(knitr)
library(pROC)
library(ggcorrplot)

load(file="00 Data/Team_split_Data.rdata") # Processed Data


# Subsetting to teams which have 5 seasons of data -- Should be 14
full_five_seasons <- list()

for(i in 1:length(final_data)){
  #rm(full_tmp)
  tmp <- final_data[[i]]
  #print(i)
  #print(nrow(tmp))
  l <- length(unique(tmp$season))
  print(l)    
  
  if(!l==5){
    next
  }
  full_five_seasons[[i]] <- tmp
}


full_five_seasons <- Filter(Negate(is.null), full_five_seasons)

### Looping through 20 teams ----

all_team_results <- list()

for(k in 1:length(full_five_seasons)){
  
  print(paste(k, " of ", length(full_five_seasons),sep=""))
  
  
  # Partitioning Team Data
  Tmp_0 <- full_five_seasons[[k]]
  
  Tmp <- Tmp_0 %>% select(-c(match_id,match_date,round,Team,Opp,Points_gained,B365_team,B365_opp,B365D)) 
  
  # removing First three records for each season 
  Tmp <- Tmp[complete.cases(Tmp), ]
  
  Tmp_train <- Tmp %>% 
    filter(season%in%c("2015-16","2016-17","2017-18","2018-19")) %>%
    select(-season) %>%
    as.data.frame() # 140 records 
  
  Tmp_test <- Tmp %>% 
    filter(season%in%c("2019-20")) %>%
    select(-season) %>% 
    as.data.frame()
  
  
  # Time Control
  myTimeControl <- trainControl(method = "timeslice",
                                initialWindow = 10,
                                horizon = 1,
                                fixedWindow = FALSE,
                                summaryFunction = mnLogLoss,
                                classProbs = TRUE)
  
  
  
  # Training multinomial model
  set.seed(555)
  multinom_fit = train(
    outcome ~ .,
    data = Tmp_train,
    method = "multinom",
    preProc = c("pca"),
    trControl = myTimeControl,
    metric="logLoss",
    trace=FALSE
  )
  
  ### Training Accuracy 
  multinom_train_acc <- table(Tmp_train$outcome==predict(multinom_fit,Tmp_train))/length(Tmp_train$outcome)
  multinom_train_acc_T <- multinom_train_acc[2]
  
  ### Test Results
  multinom_test <- predict(multinom_fit,Tmp_test)
  c1 <- confusionMatrix(multinom_test, as.factor(Tmp_test$outcome),mode = "prec_recall")
  
  # Outcome Probabilities
  multinom_test_prob <- predict(multinom_fit,Tmp_test,type="prob")
  # Test Accuracy 
  multinom_test_acc <- c1$overall[[1]]
  
  
  
  
  # Support-Vector Machine Modeling
  set.seed(555)
  svmLinear_fit = train(
    outcome ~ .,
    data = Tmp_train,
    method = "svmLinear",
    preProc = c("pca"),
    trControl = myTimeControl,
    metric="logLoss",
    trace=FALSE
  )
  
  ### Training Accuracy 
  svmLinear_train_acc <- table(Tmp_train$outcome==predict(svmLinear_fit,Tmp_train))/length(Tmp_train$outcome)
  svmLinear_train_acc_T <- svmLinear_train_acc[2]
  
  ### Test Results
  svmLinear_test <- predict(svmLinear_fit,Tmp_test)
  c2 <- confusionMatrix(svmLinear_test, as.factor(Tmp_test$outcome),mode = "prec_recall")
  
  
  # Outcome Probabilities
  svmLinear_test_prob <- predict(svmLinear_fit,Tmp_test,type="prob")
  # Test Accuracy 
  svmLinear_test_acc <- c2$overall[[1]]
  
  
  
  # C5.0 Model
  set.seed(500)
  c50_fit = train(
    outcome ~ .,
    data = Tmp_train,
    method = "C5.0",
    preProc = c("pca"),
    trControl = myTimeControl,
    metric="logLoss",
    trace=FALSE
  )
  
  ### Training Accuracy 
  c50_fit_train_acc <- table(Tmp_train$outcome==predict(c50_fit,Tmp_train))/length(Tmp_train$outcome)
  c50_fit_train_acc_T <- c50_fit_train_acc[2]
  
  ### Test Results
  c50_fit_test <- predict(c50_fit,Tmp_test)
  c2 <- confusionMatrix(c50_fit_test, as.factor(Tmp_test$outcome),mode = "prec_recall")
  
  # Outcome Probabilities
  C5_test_prob <- predict(c50_fit,Tmp_test,type="prob")
  # Test Accuracy
  c5_test_acc <- c2$overall[[1]]
  
  
  
  
  ## Ensemble - Results
  probabilities <- c(multinom_test_prob,svmLinear_test_prob,C5_test_prob)
  acc_weights <- c(multinom_train_acc_T,svmLinear_train_acc_T,c50_fit_train_acc_T)
  names(acc_weights) <- c("Multinom","SVM","C5")
  
  acc_weights2<-acc_weights/sum(acc_weights)
  
  weighted_probabilities<-((multinom_test_prob*acc_weights2[1]) + (svmLinear_test_prob*acc_weights2[2]) + (C5_test_prob*acc_weights2[3])) %>% as.data.frame()
  
  ## Function to extract column name of weighted probabilites which has the highest probability 
  
  weighted_probabilities$prediction <- colnames(weighted_probabilities)[apply(weighted_probabilities,1,which.max)]
  weighted_probabilities$actual <- Tmp_test$outcome
  
  wpf <- weighted_probabilities %>% 
    mutate(Accuracy=ifelse(actual==prediction,1,0)) %>% 
    select(actual,prediction,Accuracy,D,L,W)
  
  wpf$D <- round(wpf$D,3)
  wpf$L <- round(wpf$L,3)
  wpf$W <- round(wpf$W,3)
  
  
  Test_clubs <- Tmp_0 %>% filter(season=="2019-20") %>% select(match_date,Team,Opp) %>% as.data.frame()
  Test_clubs <- Test_clubs[4:nrow(Test_clubs),]
  
  
  wpf <- cbind(Test_clubs,wpf)
  
  
  ## Adding in betting Data
  load(file="00 Data/betting_raw.rdata")
  
  betting_team_name<-wpf$Team %>% unique()
  
  test_2019_bet <- betting_raw_out %>% 
    filter(season=="2019-20") %>% 
    filter(HomeTeam==betting_team_name|AwayTeam==betting_team_name) 
  
  test_2019_bet$ensemble_pay_out <- ifelse(test_2019_bet$result=="Away",
                                           test_2019_bet$B365A,
                                           ifelse(test_2019_bet$result=="Home",
                                                  test_2019_bet$B365H,
                                                  test_2019_bet$B365D)) 
  
  start <- 4
  end <- 4-1 + nrow(wpf)
  test_2019_bet <- test_2019_bet[start:end, ]
  
  
  wpf$ensemble_pay_out <- test_2019_bet$ensemble_pay_out
  wpf$ensemble_pay_out <- ifelse(wpf$Accuracy==1,wpf$ensemble_pay_out,-1.0)
  
  
  out <- data.frame(cut_off=NA,num_bets=NA,return=NA,profit=NA)
  
  
  cut_offs <- seq(.50,.75,by=0.01)
  
  for(i in 1:length(cut_offs)){
    wpf_t <- wpf
    tmp_prob <- cut_offs[i]
    wpf_t$prob_greater_than_cutoff <- ifelse(wpf_t$D>=tmp_prob|wpf_t$L>=tmp_prob|wpf_t$W>=tmp_prob,1,0)
    
    wpf_t <- wpf_t %>% filter(prob_greater_than_cutoff==1)
    
    num_bets <- nrow(wpf_t)
    return <- sum(wpf_t$ensemble_pay_out)
    profit <- return-num_bets
    
    out[i,1] <- tmp_prob 
    out[i,2] <- num_bets 
    out[i,3] <- return 
    out[i,4] <- profit 
  }
  
  

  Team_Results <- list()
  
  Team_Results[[1]] <- acc_weights # Model accuracies
  Team_Results[[2]] <- wpf # Ensemble predicted results
  Team_Results[[3]] <- table(wpf$Accuracy)[2]/nrow(wpf) # Ensemble accuracy
  Team_Results[[4]] <- out # payout by probabilit cut off
    
  
  all_team_results[[k]] <- Team_Results
  
  
}

save(all_team_results,file="ensmeble_results_all_teams.rdata")


  