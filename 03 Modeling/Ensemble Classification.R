library(tidyverse)
library(caret)
library(caretEnsemble)
library(corrplo)


# https://topepo.github.io/caret/pre-processing.html
# https://topepo.github.io/caret/data-splitting.html#data-splitting-for-time-series

##### Setting up Roma df ----

load(file="00 Data/Team_split_Data.rdata")


# Testing on Roma
roma <- final_data[[2]]

# Dropping any records with NA values from the beginning of each year
roma <- roma[complete.cases(roma), ]

# Dropping non-potential features
variables_to_drop <- c("match_id","match_date","season","round","Points_gained","Team","Opp")

roma <- roma %>% select(-c(variables_to_drop))

# Partitioning data
trainIndex <- createDataPartition(roma$outcome, p = .8, 
                                  list = FALSE, 
                                  times = 1)

romaTrain <- roma[trainIndex, ]
romaTest <- roma[-trainIndex, ]


## Logisitic Multinomial ----- 

# without PCA 

roma_log_mod = train(
  outcome ~ .,
  data = romaTrain,
  method = "multinom",
  trControl = trainControl(method = "cv", number = 5),
  trace = FALSE
)

roma_log_pred <- predict(roma_log_mod,romaTest)
confusionMatrix(roma_log_pred, as.factor(romaTest$outcome),mode = "prec_recall")


# with PCA improves accuracy by 15%

roma_log_mod_pca = train(
  outcome ~ .,
  data = romaTrain,
  method = "multinom",
  preProc = c("pca"),
  trControl = trainControl(method = "cv", number = 5),
  trace = FALSE
)

roma_log_pred_pca <- predict(roma_log_mod_pca,romaTest)
confusionMatrix(roma_log_pred_pca, as.factor(romaTest$outcome),mode = "prec_recall")


## SVM ----

# without pca
roma_svm_mod = train(
  outcome ~ .,
  data = romaTrain,
  method = "svmLinear",
  trControl = trainControl(method = "cv", number = 5),
  trace = FALSE
)

roma_svm_pred <- predict(roma_svm_mod,romaTest)
confusionMatrix(roma_svm_pred, as.factor(romaTest$outcome))

# without pca
roma_svm_mod_pca = train(
  outcome ~ .,
  data = romaTrain,
  method = "svmLinear",
  preProc = c("pca"),
  trControl = trainControl(method = "cv", number = 5),
  trace = FALSE
)

roma_svm_pred_pca <- predict(roma_svm_mod_pca,romaTest)
confusionMatrix(roma_svm_pred_pca, as.factor(romaTest$outcome))


## Naive-Bayes


# without pca
roma_nb_mod = train(
  outcome ~ .,
  data = romaTrain,
  method = "naive_bayes",
  trControl = trainControl(method = "cv", number = 5),
  trace = FALSE
)

roma_nb_pred <- predict(roma_nb_mod,romaTest)
confusionMatrix(roma_nb_pred, as.factor(romaTest$outcome))

# without pca
roma_nb_mod_pca = train(
  outcome ~ .,
  data = romaTrain,
  method = "naive_bayes",
  preProc = c("pca"),
  trControl = trainControl(method = "cv", number = 5),
  trace = FALSE
)

roma_nb_pred_pca <- predict(roma_nb_mod_pca,romaTest)
confusionMatrix(roma_nb_pred_pca, as.factor(romaTest$outcome))


### Time Slices ----
# https://topepo.github.io/caret/data-splitting.html#time
# https://stackoverflow.com/questions/24758218/time-series-data-splitting-and-model-evaluation

timeSlices <- createTimeSlices(1:nrow(romaTrain), 
                               initialWindow = 100, # Initial dataset
                               horizon = 1, # Predicting one match ahead
                               fixedWindow = FALSE) # Ensuring cumulative training set)

#trainSlices <- timeSlices[[1]]
#testSlices <- timeSlices[[2]]

myTimeControl <- trainControl(method="timeslice",
                              initialWindow = 100,
                              horizon = 1,
                              fixedWindow = FALSE)

logMod <- train(outcome ~ .,
                    data=romaTrain,
                    method="multinom",
                    preProc=c("pca"),
                    trControl = myTimeControl)

logPreds <-predict(logPred, romaTest)



