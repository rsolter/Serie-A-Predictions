## Modeling Winter 19 **Sandbox**


## Set up ----

library(tidyverse)
library(caret)
library(randomForest)

load(file="2019 Winter Approach/processed.rdata")

# isolating to complete cases (removing the first few records where form is unaviable)
processed <- processed[complete.cases(processed), ]

# nesting dataframe by team
nested_df <- processed %>% group_by(Team_name) %>% nest()


## Ovearll Test/Train Stats ---
overall_train <- processed %>% filter(season%in%c("2016-17","2017-18"))
overall_test <- processed %>% filter(season%in%c("2018-19"))

# Comparing outcome distributions (overall and by team) between test and training set 
table(overall_train$outcome)/nrow(overall_train)*100
table(overall_test$outcome)/nrow(overall_test)*100

overall_train %>% group_by(Team_name,Team_points) %>% tally()
overall_test %>% group_by(Team_name,Team_points) %>% tally()




## Classification Models to try
  # RandomForest
  # SVM
  # NaiveBayes
  # NeuralNet
  # Multinomial Regression


## Random Forest ----

  # Random forests use a ensemble of decision trees, each of which represents a subset of the predictors
  # https://www.guru99.com/r-random-forest-tutorial.html
  # https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/tutorial-random-forest-parameter-tuning-r/tutorial/


# Subsetting to teams which appear in all 3 seasons 
threeSeasonteams <- processed %>% 
  select(Team_name,season) %>% 
  unique() %>% group_by(Team_name) %>% 
  tally() %>% filter(n==3)
nested_df_3_season <- nested_df %>% filter(Team_name%in%threeSeasonteams$Team_name)


rfResults <- list()

for(i in 1:nrow(nested_df_3_season)){
  
  # isolating team, team record data
  tmp_team <- nested_df_3_season$Team_name[[i]] %>% as.character()
  tmp_data <- nested_df_3_season$data[[i]] %>% as.data.frame() %>%
    select(-match_id,-round,-Team_points)
  
  # partitions 
  tmp_train <- tmp_data %>% filter(season%in%c("2016-17","2017-18"))
  tmp_test <- tmp_data %>% filter(season%in%c("2018-19"))
  
  # Model
  model1 <- randomForest(outcome ~ ., data = tmp_train, 
                         ntree=1000, # number of trees
                         mtry=12, # number of randomly selected features
                         importance = TRUE)

  test1 <- predict(model1, tmp_test, type = "class")
  print(tmp_team)
  overallAcc<-(table(test1==tmp_test$outcome)[[2]])/nrow(tmp_test)
  tableOut<-table(test1,tmp_test$outcome)
  print(overallAcc)
  
  Importance_Plot <- varImpPlot(model1)
  
  # round by round accuracy
  round_by_round_acc <- data.frame(actual=tmp_test$outcome,predicted=test1)
  round_by_round_acc$correct <- ifelse(round_by_round_acc$actual==round_by_round_acc$predicted,1,0)
  round_by_round_acc <- round_by_round_acc %>%
    mutate(cum_acc = cumsum(correct)/row_number())
  
  results_out <- list(model1,overallAcc,Importance_Plot,tableOut,round_by_round_acc)
  names(results_out) <- c("Model","Overall Accuracy","Importance_Plot","Confusion Table","RoundBYRound")
  
  rfResults[[i]] <- results_out
  names(rfResults[[i]]) <- tmp_team
  
  rm(tmp_team,tmp_data,tmp_train,tmp_test,model1,test1,overallAcc,tableOut,Importance_Plot,
     round_by_round_acc,results_out,i)
  
}

# https://www.quora.com/What-are-the-advantages-and-disadvantages-for-a-random-forest-algorithm
# https://towardsdatascience.com/light-on-math-machine-learning-intuitive-guide-to-understanding-decision-trees-adb2165ccab7



## Visualizing outcome by Round

rBrOut <- list()
for(k in 1:length(rfResults)){
  teamname <- rfResults[[k]][1] %>% names()
  round_results<-rfResults[[k]][[5]]$cum_acc %>% as.vector()
  
  out <- data.frame(team=teamname,round=NA,cum_acc=round_results)
  out$team <- teamname
  out$round <- 1:nrow(out)
  
  rBrOut[[k]] <- out
  
  rm(teamname,round_results,out,k)
}

RbR <- bind_rows(rBrOut)
rm(rBrOut)

ggplot(RbR, aes(x=round,y=cum_acc,)) + geom_line() + 
  theme_minimal() + facet_wrap(facets=vars(team)) + 
  geom_hline(yintercept = 0.5,color="grey",linetype=2) +
  ggtitle("Random Forest Performance By Round",
          subtitle = "Only teams with 3 seasons of data included") +
  xlab("Round")+ ylab("Cumulative Prediction Accuracy")

# Questions this raises..
  # Is this model only good at picking out teams that win/lose majority of their matches?
  # How would other modeling techniques look on this same graph?
  # What kind of outcomes are we especially bad at predicting
  # Interrogate the models. Which variables are the most imporant? 


### K-nearest neighbor 

# https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
# https://dataaspirant.com/2017/01/09/knn-implementation-r-using-caret-package/

library(kknn)
library(class)

#preProcValues <- preProcess(overall_train, method = c("center", "scale"))
#trainTransformed <- predict(preProcValues, overall_train)
#testTransformed <- predict(preProcValues, overall_test)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

knn_fit <- caret::train(outcome ~., data = overall_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
summary(knn_fit)

test_pred <- predict(knn_fit, newdata = overall_test)

confusionMatrix(test_pred,overall_test$outcome)



knnResults <- list()

for(i in 1:nrow(nested_df_3_season)){
  
  # isolating team, team record data
  tmp_team <- nested_df_3_season$Team_name[[i]] %>% as.character()
  tmp_data <- nested_df_3_season$data[[i]] %>% as.data.frame() %>%
    select(-match_id,-round,-Team_points)
  
  # partitions 
  tmp_train <- tmp_data %>% filter(season%in%c("2016-17","2017-18"))
  tmp_test <- tmp_data %>% filter(season%in%c("2018-19"))
  
  # Model
  model1 <- kknn:kknn(outcome ~ ., train=tmp_train, test=tmp_test)
  
  test1 <- predict(model1, tmp_test, type = "class")
  print(tmp_team)
  overallAcc<-(table(test1==tmp_test$outcome)[[2]])/nrow(tmp_test)
  tableOut<-table(test1,tmp_test$outcome)
  print(overallAcc)
  
  Importance_Plot <- varImpPlot(model1)
  
  # round by round accuracy
  round_by_round_acc <- data.frame(actual=tmp_test$outcome,predicted=test1)s
  round_by_round_acc$correct <- ifelse(round_by_round_acc$actual==round_by_round_acc$predicted,1,0)
  round_by_round_acc <- round_by_round_acc %>%
    mutate(cum_acc = cumsum(correct)/row_number())
  
  results_out <- list(model1,overallAcc,Importance_Plot,tableOut,round_by_round_acc)
  names(results_out) <- c("Model","Overall Accuracy","Importance_Plot","Confusion Table","RoundBYRound")
  
  rfResults[[i]] <- results_out
  names(rfResults[[i]]) <- tmp_team
  
  rm(tmp_team,tmp_data,tmp_train,tmp_test,model1,test1,overallAcc,tableOut,Importance_Plot,
     round_by_round_acc,results_out,i)
  
}


