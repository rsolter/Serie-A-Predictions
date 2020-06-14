------------------------------------------------------------------------

### 1. Gathering Data

The data for this project was gathered from the official [Serie A
website](http://www.legaseriea.it/en) and its match reports from the
2015-16 season through the current 2019-20 season. Note that due to the
effects of the Covid-19, all matches were postponed in Italy after the
first week of March, 2020. The scrapers used the `rvest` package and can
be found
[here](https://github.com/rsolter/Serie-A-Predictions/tree/master/01%20Scrapers).
Weekly [Elo](https://en.wikipedia.org/wiki/Elo_rating_system) scores for
each team were also downloaded from the website Clubelo.com using their
[API](http://clubelo.com/API).

**Initial feature list:**

|                  |                                  |                  |
|------------------|----------------------------------|------------------|
| Goals            | Total Shots                      | Attacks (Middle) |
| Saves            | Shots on Target                  | Attacks (Left)   |
| Penalties        | Shots on Target from Free Kicks  | Attacks (Right)  |
| Fouls            | Shots off Target                 | Fast Breaks      |
| Red Cards        | Shots off Target from Free Kicks | Crosses          |
| Yellow Cards     | Shots from within the Box        | Long Balls       |
| Key Passes       | Shots on Target from Set Pieces  | Possession       |
| Completed Passes | Shots off Target from Set Pieces | Corners          |
| Passing Accuracy | Scoring Chances                  | Offsides         |

#### A Note on Expected goals

[Expected
Goals](https://wikieducator.org/Sport_Informatics_and_Analytics/Performance_Monitoring/Expected_Goals)
(*xG*) are not included in this analysis. xG measures the quality of
goalscoring chances and the likelihood of them being scored. Factors
influencing the probability of a goal being scored from a shot include
distance from the goal; angle from the goal; and whether or not the
player taking the shot was at least 1 m away from the nearest defender.
Although very popular, I have not included these stats in the analysis
but may do so later from a site like [understat](https://understat.com/)

------------------------------------------------------------------------

### 2. Processing the Data

In its raw form the observations gathered are grouped by match, with
stats for both the home and away teams. Below is an example of the top
five records.

<table>
<thead>
<tr>
<th style="text-align:left;">
Team\_h
</th>
<th style="text-align:left;">
Team\_a
</th>
<th style="text-align:right;">
goals\_h
</th>
<th style="text-align:right;">
goals\_a
</th>
<th style="text-align:right;">
saves\_h
</th>
<th style="text-align:right;">
saves\_a
</th>
<th style="text-align:right;">
pen\_h
</th>
<th style="text-align:right;">
pen\_a
</th>
<th style="text-align:right;">
shots\_h
</th>
<th style="text-align:right;">
shots\_a
</th>
<th style="text-align:right;">
shots\_on\_h
</th>
<th style="text-align:right;">
shots\_on\_a
</th>
<th style="text-align:right;">
shot\_on\_fk\_h
</th>
<th style="text-align:right;">
shot\_on\_fk\_a
</th>
<th style="text-align:right;">
shots\_off\_h
</th>
<th style="text-align:right;">
shots\_off\_a
</th>
<th style="text-align:right;">
shot\_off\_fk\_h
</th>
<th style="text-align:right;">
shot\_off\_fk\_a
</th>
<th style="text-align:right;">
shots\_box\_h
</th>
<th style="text-align:right;">
shots\_box\_a
</th>
<th style="text-align:right;">
shots\_sp\_on\_h
</th>
<th style="text-align:right;">
shots\_sp\_on\_a
</th>
<th style="text-align:right;">
fouls\_h
</th>
<th style="text-align:right;">
fouls\_a
</th>
<th style="text-align:right;">
scoring\_chances\_h
</th>
<th style="text-align:right;">
scoring\_chances\_a
</th>
<th style="text-align:right;">
offsides\_h
</th>
<th style="text-align:right;">
offsides\_a
</th>
<th style="text-align:right;">
corners\_h
</th>
<th style="text-align:right;">
corners\_a
</th>
<th style="text-align:right;">
yellow\_h
</th>
<th style="text-align:right;">
yellow\_a
</th>
<th style="text-align:right;">
shots\_sp\_off\_h
</th>
<th style="text-align:right;">
shots\_sp\_off\_a
</th>
<th style="text-align:right;">
fast\_breaks\_h
</th>
<th style="text-align:right;">
fast\_breaks\_a
</th>
<th style="text-align:left;">
season
</th>
<th style="text-align:left;">
round
</th>
<th style="text-align:right;">
poss\_h
</th>
<th style="text-align:right;">
poss\_a
</th>
<th style="text-align:left;">
match\_date
</th>
<th style="text-align:right;">
attacks\_h
</th>
<th style="text-align:right;">
attacks\_a
</th>
<th style="text-align:right;">
match\_id
</th>
<th style="text-align:left;">
outcome
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
HellasVerona
</td>
<td style="text-align:left;">
Roma
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
2015-16
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:left;">
2015-08-22
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
D
</td>
</tr>
<tr>
<td style="text-align:left;">
Lazio
</td>
<td style="text-align:left;">
Bologna
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
2015-16
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:left;">
2015-08-22
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
H
</td>
</tr>
<tr>
<td style="text-align:left;">
Juventus
</td>
<td style="text-align:left;">
Udinese
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
2015-16
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:left;">
2015-08-23
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
A
</td>
</tr>
<tr>
<td style="text-align:left;">
Empoli
</td>
<td style="text-align:left;">
Chievoverona
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
2015-16
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:left;">
2015-08-23
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
A
</td>
</tr>
<tr>
<td style="text-align:left;">
Fiorentina
</td>
<td style="text-align:left;">
Milan
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
2015-16
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:left;">
2015-08-23
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
H
</td>
</tr>
</tbody>
</table>
From this raw form the data has been processed in the following ways:

#### Replacing data with lagged averages

All the stats collected (with the exception of Elo), have been replaced
with lagged averages. The rationale for this is that we need historical
performance data to try and predict future match outcomes.

#### Data regrouped by team

The full set of records is broken up by teams so that there exist +20
datasets. One for each individual team with lagged average stats on
their and their opponents performance.

#### Splitting data

From the five season in the dataset, seasons 2015-16, 2016-17, and
2017-18 are used for training, the 2018-19 season will searve as the
validation set, and the 2019-20 season will serve as the holdout data.

As the matches occur in chronological order, each dataset will be broken
apart in such a way that the model will be built on the first *n*
observations and tested on the *n+1* match. This is accomplished using
the *time\_slice()* function in the **caret** package. A visual
representation of this partition can be seen below in the bottom left
quadrant. In the example below, there is a time series with 20 data
points. We can fix initialWindow = 5 and look at different settings of
the other two arguments. In the plot below, rows in each panel
correspond to different data splits (i.e. resamples) and the columns
correspond to different data points. Also, red indicates samples that
are in included in the training set and the blue indicates samples in
the test set. See more
[here](http://topepo.github.io/caret/data-splitting.html#data-splitting-for-time-series).

![](/assets/images/Split_time-1.svg)

------------------------------------------------------------------------

### 3. Feature Engineering

Before starting any modeling, there are some data process and feature
engineering steps to take on:

#### Feature selection with Random Forest

There are a lot of variables collected in the match report that are
likely not predictive of a matches outcome. To remove those from the
dataset, a random forest is used to determine which variables are
relatively unimportant. Ultimately, I drop information about penalties,
free kick shots off target, shots on target from free kicks, and
information about shots taken from set pieces.

In contrast, it appears that the number of shots within the penalty box,
total shots on target, and overall numbers of attacks are the most
predictive of match outcome.

![](SerieA_Blog_Post_files/figure-markdown_github/Feature%20Selection%20using%20Random%20Forest-1.png)

#### Feature Extraction with PCA

Even after removing 10 features from the dataset, there are still a
large number of predictors for each match’s outcome. To reduce the
number of features while maximizing the amount of variation still
explained, principal components analysis was applied as a
[pre-processing
technique](https://topepo.github.io/caret/pre-processing.html#transforming-predictors)
in caret.

------------------------------------------------------------------------

### 3. Distribution of Outcomes by Team

It’s worthwhile to point out that the distribution of outcomes is
naturally different by team. Dominant teams like Juventus, Napoli, Roma,
and Inter Milan all have win percentages over 50%. This class imbalance
has consequences for the models built. However, for the example below,
we’ll focus on Sampdoria which has a relatively balanced distribution of
outcomes for seasons 2015-16 - 2018-19: 34.8% Win, 23.6%, Loss 41.4%.

![](SerieA_Blog_Post_files/figure-markdown_github/outcome_viz-1.png)

### 4. Illustrative Example with U.C Sampdoria

The records for Sampdoria have been broken apart into three datasets:
`Samp_train` with records from the “2015-16”,“2016-17”,“2017-18”
seasons, `Samp_validation` with records from the “2018-19” season, and
`Samp_holdout` from the ’2019-20\` season. Each dataset has been
scrubbed of the first three records from each season as they do not have
lagged average values for the various features.

Various models will be trained on the `Samp_train` set, their parameters
tested and tuned on `Samp_validation`, and all will be tested
individually and in an ensemble on the `Samp_holdout` set.

``` r
# Partitioning Sampdoria Data

Samp <- final_data[[17]]

Samp <- Samp[complete.cases(Samp), ]

Samp_train <- Samp %>% 
  filter(season%in%c("2015-16","2016-17","2017-18")) %>%
  select(-c(match_id,match_date,season,round,Team,Opp,Points_gained)) %>%
  as.data.frame() # 105 records 

Samp_validation <- Samp %>% 
  filter(season%in%c("2018-19")) %>%
  select(-c(match_id,match_date,season,round,Team,Opp,Points_gained)) # 35 records

Samp_holdout <- Samp %>% 
  filter(season%in%c("2019-20")) %>%
  select(-c(match_id,match_date,season,round,Team,Opp,Points_gained)) # 21 records
```

#### Multinomial Logistic Regression

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  D  L  W
    ##          D  0  0  0
    ##          L  2  3  0
    ##          W  6 11 13
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.4571          
    ##                  95% CI : (0.2883, 0.6335)
    ##     No Information Rate : 0.4             
    ##     P-Value [Acc > NIR] : 0.2997392       
    ##                                           
    ##                   Kappa : 0.1307          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.0002734       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: D Class: L Class: W
    ## Precision                  NA  0.60000   0.4333
    ## Recall                 0.0000  0.21429   1.0000
    ## F1                         NA  0.31579   0.6047
    ## Prevalence             0.2286  0.40000   0.3714
    ## Detection Rate         0.0000  0.08571   0.3714
    ## Detection Prevalence   0.0000  0.14286   0.8571
    ## Balanced Accuracy      0.5000  0.55952   0.6136

![](SerieA_Blog_Post_files/figure-markdown_github/Multinomial%20Regression-1.png)

#### SVM

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction D L W
    ##          D 3 4 4
    ##          L 1 1 0
    ##          W 4 9 9
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.3714          
    ##                  95% CI : (0.2147, 0.5508)
    ##     No Information Rate : 0.4             
    ##     P-Value [Acc > NIR] : 0.69427         
    ##                                           
    ##                   Kappa : 0.0644          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.01286         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: D Class: L Class: W
    ## Precision             0.27273  0.50000   0.4091
    ## Recall                0.37500  0.07143   0.6923
    ## F1                    0.31579  0.12500   0.5143
    ## Prevalence            0.22857  0.40000   0.3714
    ## Detection Rate        0.08571  0.02857   0.2571
    ## Detection Prevalence  0.31429  0.05714   0.6286
    ## Balanced Accuracy     0.53935  0.51190   0.5507

![](SerieA_Blog_Post_files/figure-markdown_github/SVM-1.png)

#### Random Forest

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  D  L  W
    ##          D  0  0  0
    ##          L  2  4  0
    ##          W  6 10 13
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.4857          
    ##                  95% CI : (0.3138, 0.6601)
    ##     No Information Rate : 0.4             
    ##     P-Value [Acc > NIR] : 0.1934835       
    ##                                           
    ##                   Kappa : 0.1754          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.0004398       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: D Class: L Class: W
    ## Precision                  NA   0.6667   0.4483
    ## Recall                 0.0000   0.2857   1.0000
    ## F1                         NA   0.4000   0.6190
    ## Prevalence             0.2286   0.4000   0.3714
    ## Detection Rate         0.0000   0.1143   0.3714
    ## Detection Prevalence   0.0000   0.1714   0.8286
    ## Balanced Accuracy      0.5000   0.5952   0.6364

![](SerieA_Blog_Post_files/figure-markdown_github/RandomForest-1.png)

#### Naive-Bayes

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  D  L  W
    ##          D  8 13 13
    ##          L  0  1  0
    ##          W  0  0  0
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.2571          
    ##                  95% CI : (0.1249, 0.4326)
    ##     No Information Rate : 0.4             
    ##     P-Value [Acc > NIR] : 0.974           
    ##                                           
    ##                   Kappa : 0.0309          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: D Class: L Class: W
    ## Precision              0.2353  1.00000       NA
    ## Recall                 1.0000  0.07143   0.0000
    ## F1                     0.3810  0.13333       NA
    ## Prevalence             0.2286  0.40000   0.3714
    ## Detection Rate         0.2286  0.02857   0.0000
    ## Detection Prevalence   0.9714  0.02857   0.0000
    ## Balanced Accuracy      0.5185  0.53571   0.5000

![](SerieA_Blog_Post_files/figure-markdown_github/Naive-Bayes-1.png)

#### Ensemble

#### Results

------------------------------------------------------------------------

### 5. Overall Results

### 6. Conclusion and Next Steps

-   Include xG data

    -   Find a way to account for class imbalance

    -   Add player-level data

    -   Try a generalizable model
