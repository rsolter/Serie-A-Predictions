########## Modeling Winter '19
library(tidyverse)
library(caret)

load(file="2019 Winter Approach/processed.rdata")

nested_df <- processed %>% group_by(Team_name) %>% nest()


## Classification Models to try
  # RandomForest
  # SVM
  # NaiveBayes
  # NeuralNet
  # Multinomial Regression


## Partitioning --> Need to break apart the data 

# Splitting of the data into training and test sets is done among the 
# three most recent seasons. 2016-17 and 2017-18 are used for training, 
# while the 2018-19 season is used for testing model accuracy. 


# The following steps take place before splitting the data:
  
#  Checking for zero and near-zero variance explanatory variables
#  Removing highlighy correlated variables
#  Removing specific variables (Team and opponent names, red cards for both home and away teams)

test <- nested_df %>% filter(Team_name=="Milan") %>% select(data)
test <- test[[1]] %>% as.data.frame()

## Just Running Some Models


