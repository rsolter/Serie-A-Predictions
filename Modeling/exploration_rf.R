library(tidyverse)
library(DataExplorer)
library(broom)
library(e1071)
library(caret)
source(file = "Processing_and_Engineering/engineering_seriea_design_matrix.R")

set.seed(1234)

#DF_DataExplorer_Report <- DF %>% select(-`Away Team`,-`Home Team`,-season,-round,-game_id) 
#DataExplorer::create_report(data = DF_DataExplorer_Report, y = "result")


#### -- RF of DF ---

# Without using the trailing data, the overall accuracy rate of the model is around 60%

#https://uc-r.github.io/random_forests
library(ranger)
DF_new <- DF %>% select(-game_id,-goals_h,-goals_a) # removing goals, game_id
DF_new_complete <- preProcess(DF, method = "knnImpute")
names(DF_new_complete) <- make.names(names(DF_new_complete))


# Ranger
train <- DF_new_complete %>% filter(!season=="2018-19")
test <- DF_new_complete %>% filter(season=="2018-19")

features<-names(train)

SerieARanger <- ranger(
  formula   = result ~ ., 
  data      = train, 
  num.trees = 500,
  mtry      = floor(length(features) / 3)
)

## OOB Prediction Erro - 38.91%

p <- predict(SerieARanger,test)

caret::confusionMatrix(p$predictions,test$result)

## Overall accuracy is around 0.587
## Draw accuracy is very low (0.15), compared to Home and Away (~0.8)


# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(20, 50, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

# Total # of combinations
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula   = result ~ ., 
    data      = train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    #    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

## 67%-69% error rate

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)



# Optimal Tree

OOB_RMSE <- vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula   = result ~ ., 
    data      = train, 
    num.trees       = 500,
    mtry            = 30,
    min.node.size   = 7,
    #    sample.fraction = .8,
    importance      = 'impurity'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}


## Error rates visualized
hist(OOB_RMSE, breaks = 20)

## League Rank by far the most valuable variable
## Lead by scoring chances, shots in the box, league rank, shonts on, balls won ..
optimal_ranger$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 25 important variables")



pred_ranger<-predict(optimal_ranger,test)

caret::confusionMatrix(pred_ranger$predictions,test$result) 

# Overall Accuracy is near 60%. With Draw at 18% and Home/Away near 78-79%



#### Most Important variables
  # Scoring chances a
  # shots in the box
  # shots on
  # shots
  # rank vars
  # form vars
  # balls won
  # balls lost




























#### -- RF of DF_Trailing ---
## With Trailing

source(file = "Processing_and_Engineering//engineering_seriea_design_matrix trailing.R",
       verbose = F)

DF_trailing_test <- DF_trailing %>% select(-game_id,-points)
DF_trailing_test <- DF_trailing_test[complete.cases(DF_trailing_test), ]
trail_train <- DF_trailing_test %>% filter(!season=="2018-19")
trail_test <- DF_trailing_test %>% filter(season=="2018-19")

trail_features<-names(trail_train)

Trail_SerieARanger <- ranger(
  formula   = result ~ ., 
  data      = trail_train, 
  num.trees = 500,
  mtry      = floor(length(trail_features) / 3)
)

## OOB Prediction Erro - 45.18%


## Tuning Parameters

  # ntree - number of trees
  # mtry - the number of variables to randomly sample as candidates at each split.
  #  A common suggestion is to start with 5 values evenly spaced across the range from 2 to p.
  # samplesize - the number of samples to train on
  # nodesize - minimum number of samples within the terminal nodes. Controls 
  #  the complexity of the trees. Smaller node size allows for deeper, 
  #  more complex trees and smaller node results in shallower trees.
  # maxnodes - maximum number of terminal nodes. Another way to control the complexity



# Full Grid Search with Ranger

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(20, 50, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

# Total # of combinations
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula   = result ~ ., 
    data      = trail_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
#    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

## 66%-68% error rate

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)


# Optimal Tree

OOB_RMSE <- vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula   = result ~ ., 
    data      = trail_train, 
    num.trees       = 500,
    mtry            = 28,
    min.node.size   = 5,
#    sample.fraction = .8,
    importance      = 'impurity'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}


## Error rates visualized
hist(OOB_RMSE, breaks = 20)

optimal_ranger$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 25 important variables")

## League Rank by far the most valuable variable

pred_ranger<-predict(optimal_ranger,trail_test)

table(pred_ranger$predictions==trail_test$result) # 180/(188+180) -48.9

caret::confusionMatrix(pred_ranger$predictions,trail_test$result) 














## multinom - nnet
# https://www.youtube.com/watch?v=QvnsTXfPenU

# glmnet - glmnet
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#log
# https://amunategui.github.io/binary-outcome-modeling/


glmNet1_results <- train(trainDF[,predictorsNames], trainDF[,outcomeName], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))



# svm - e1071	or kernlab
# https://medium.com/@ODSC/build-a-multi-class-support-vector-machine-in-r-abcdd4b7dab6

svm1 <- e1071::svm(result~., data=trail_train, 
            method="C-classification", kernal="radial", 
            gamma=0.1, cost=10)

svm1_results<-predict(svm1,trail_test)
confusionMatrix(svm1_results,trail_test$result)



# neural_net - nnet

# gradient boosted tree - xgboost




glmnet_fit <- glmnet(x, , family = "multinomial", type.multinomial = "grouped")


