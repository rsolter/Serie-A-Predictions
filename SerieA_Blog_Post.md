### 1. Objective

The goal of this post is to try and predict the outcome of soccer
matches in the Italian top-flight division, *Serie A*. There are three
possible outcomes for each match: a home win, an away win, or a draw. To
make things more interesting, the predicted outcomes and their
associated probabilities will be compared to historical odds offered by
bookmakers in Europe which were gathered from
<https://www.football-data.co.uk>.

As an example, the odds given for a single match between Parma and
Juventus in August, 2019 are listed below along with those same odds
converted to probabilities. These odds were offered by the company
Bet365.

<table>
<caption>
Example Odds from Bet365
</caption>
<thead>
<tr>
<th style="text-align:left;">
Date
</th>
<th style="text-align:left;">
HomeTeam
</th>
<th style="text-align:left;">
AwayTeam
</th>
<th style="text-align:right;">
B365H
</th>
<th style="text-align:right;">
B365A
</th>
<th style="text-align:right;">
B365D
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
24/08/2019
</td>
<td style="text-align:left;">
Parma
</td>
<td style="text-align:left;">
Juventus
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
1.33
</td>
<td style="text-align:right;">
5.25
</td>
</tr>
</tbody>
</table>
<table>
<caption>
Converted to Probabilities
</caption>
<thead>
<tr>
<th style="text-align:left;">
Date
</th>
<th style="text-align:left;">
HomeTeam
</th>
<th style="text-align:left;">
AwayTeam
</th>
<th style="text-align:right;">
B365H
</th>
<th style="text-align:right;">
B365A
</th>
<th style="text-align:right;">
B365D
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
24/08/2019
</td>
<td style="text-align:left;">
Parma
</td>
<td style="text-align:left;">
Juventus
</td>
<td style="text-align:right;">
0.1111111
</td>
<td style="text-align:right;">
0.7518797
</td>
<td style="text-align:right;">
0.1904762
</td>
</tr>
</tbody>
</table>
Note that the sum of the three probabilities is equal to 1.0534, so the
odds offered by Bet365 are not true odds. This practice is standard
among all odds offered by bookmakers. The ‘extra’ 5.34% is known as “the
vig” and helps bookmakers ensure a profit across all the matches on
which they’re offering odds. Wikipedia has a better explanation of how
the vig plays out
[here](https://en.wikipedia.org/wiki/Vigorish#The_simplest_wager).

------------------------------------------------------------------------

### 2. Gathering Data

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

### 3. Processing the Data

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

#### Data regrouped by team

The full set of records is broken up by teams so that there exist +20
datasets, one for each team with observations ordered chronologically.
The reason for doing this is that each team has its history and
distribution of outcomes, so it makes sense to try and build a set of
models for each team. In this regrouping the variables have been given
’\_team’ and ’\_opp’ suffixes to refer to the team and its opponents
statistics.

#### Replacing data with lagged averages

All the stats collected (with the exception of Elo), have been replaced
with lagged averages from the previous 3 matches. The rationale for this
is that we need historical performance data to try and predict future
match outcomes Below is an example cut of the data from SS Lazio:

``` r
ss_lazio <- final_data[[3]] %>% head(5)

knitr::kable(ss_lazio[1:5,])
```

<table>
<thead>
<tr>
<th style="text-align:right;">
match\_id
</th>
<th style="text-align:left;">
match\_date
</th>
<th style="text-align:left;">
season
</th>
<th style="text-align:left;">
round
</th>
<th style="text-align:left;">
Team
</th>
<th style="text-align:left;">
Opp
</th>
<th style="text-align:left;">
home\_match
</th>
<th style="text-align:left;">
outcome
</th>
<th style="text-align:right;">
Points\_gained
</th>
<th style="text-align:right;">
goals\_team
</th>
<th style="text-align:right;">
saves\_team
</th>
<th style="text-align:right;">
shots\_team
</th>
<th style="text-align:right;">
shots\_on\_team
</th>
<th style="text-align:right;">
shots\_off\_team
</th>
<th style="text-align:right;">
shots\_box\_team
</th>
<th style="text-align:right;">
fouls\_team
</th>
<th style="text-align:right;">
scoring\_chances\_team
</th>
<th style="text-align:right;">
offsides\_team
</th>
<th style="text-align:right;">
corners\_team
</th>
<th style="text-align:right;">
yellow\_team
</th>
<th style="text-align:right;">
fast\_breaks\_team
</th>
<th style="text-align:right;">
poss\_team
</th>
<th style="text-align:right;">
attacks\_team
</th>
<th style="text-align:right;">
Elo\_team
</th>
<th style="text-align:right;">
goals\_opp
</th>
<th style="text-align:right;">
saves\_opp
</th>
<th style="text-align:right;">
shots\_opp
</th>
<th style="text-align:right;">
shots\_on\_opp
</th>
<th style="text-align:right;">
shots\_off\_opp
</th>
<th style="text-align:right;">
shots\_box\_opp
</th>
<th style="text-align:right;">
fouls\_opp
</th>
<th style="text-align:right;">
scoring\_chances\_opp
</th>
<th style="text-align:right;">
offsides\_opp
</th>
<th style="text-align:right;">
corners\_opp
</th>
<th style="text-align:right;">
yellow\_opp
</th>
<th style="text-align:right;">
fast\_breaks\_opp
</th>
<th style="text-align:right;">
poss\_opp
</th>
<th style="text-align:right;">
attacks\_opp
</th>
<th style="text-align:right;">
Elo\_opp
</th>
<th style="text-align:right;">
B365\_team
</th>
<th style="text-align:right;">
B365\_opp
</th>
<th style="text-align:right;">
B365D
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
2015-08-22
</td>
<td style="text-align:left;">
2015-16
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
Lazio
</td>
<td style="text-align:left;">
Bologna
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
W
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1751.378
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1486.670
</td>
<td style="text-align:right;">
0.6666667
</td>
<td style="text-align:right;">
0.1428571
</td>
<td style="text-align:right;">
0.2380952
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
2015-08-30
</td>
<td style="text-align:left;">
2015-16
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
Lazio
</td>
<td style="text-align:left;">
Chievoverona
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
L
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1730.399
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1595.023
</td>
<td style="text-align:right;">
0.2941176
</td>
<td style="text-align:right;">
0.4545455
</td>
<td style="text-align:right;">
0.3030303
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
2015-09-13
</td>
<td style="text-align:left;">
2015-16
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Lazio
</td>
<td style="text-align:left;">
Udinese
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
W
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1709.957
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1576.308
</td>
<td style="text-align:right;">
0.6172840
</td>
<td style="text-align:right;">
0.1666667
</td>
<td style="text-align:right;">
0.2666667
</td>
</tr>
<tr>
<td style="text-align:right;">
40
</td>
<td style="text-align:left;">
2015-09-20
</td>
<td style="text-align:left;">
2015-16
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Lazio
</td>
<td style="text-align:left;">
Napoli
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
L
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.3333333
</td>
<td style="text-align:right;">
3.333333
</td>
<td style="text-align:right;">
13.66667
</td>
<td style="text-align:right;">
7.666667
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
4.000000
</td>
<td style="text-align:right;">
14.33333
</td>
<td style="text-align:right;">
9.333333
</td>
<td style="text-align:right;">
1.000000
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2.666667
</td>
<td style="text-align:right;">
0.5766667
</td>
<td style="text-align:right;">
32.33333
</td>
<td style="text-align:right;">
1720.819
</td>
<td style="text-align:right;">
1.6666667
</td>
<td style="text-align:right;">
2.000000
</td>
<td style="text-align:right;">
14.000000
</td>
<td style="text-align:right;">
5.333333
</td>
<td style="text-align:right;">
8.666667
</td>
<td style="text-align:right;">
2.666667
</td>
<td style="text-align:right;">
14.00000
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
2.666667
</td>
<td style="text-align:right;">
3.333333
</td>
<td style="text-align:right;">
2.000000
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.5466667
</td>
<td style="text-align:right;">
27.33333
</td>
<td style="text-align:right;">
1737.392
</td>
<td style="text-align:right;">
0.5235602
</td>
<td style="text-align:right;">
0.2500000
</td>
<td style="text-align:right;">
0.2777778
</td>
</tr>
<tr>
<td style="text-align:right;">
47
</td>
<td style="text-align:left;">
2015-09-23
</td>
<td style="text-align:left;">
2015-16
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
Lazio
</td>
<td style="text-align:left;">
Genoa
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
W
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.6666667
</td>
<td style="text-align:right;">
3.666667
</td>
<td style="text-align:right;">
10.00000
</td>
<td style="text-align:right;">
5.000000
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3.666667
</td>
<td style="text-align:right;">
13.66667
</td>
<td style="text-align:right;">
7.000000
</td>
<td style="text-align:right;">
1.333333
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3.333333
</td>
<td style="text-align:right;">
0.5100000
</td>
<td style="text-align:right;">
25.00000
</td>
<td style="text-align:right;">
1708.079
</td>
<td style="text-align:right;">
0.6666667
</td>
<td style="text-align:right;">
2.333333
</td>
<td style="text-align:right;">
8.333333
</td>
<td style="text-align:right;">
2.666667
</td>
<td style="text-align:right;">
5.666667
</td>
<td style="text-align:right;">
1.333333
</td>
<td style="text-align:right;">
19.33333
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2.000000
</td>
<td style="text-align:right;">
2.333333
</td>
<td style="text-align:right;">
2.333333
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.5433333
</td>
<td style="text-align:right;">
18.66667
</td>
<td style="text-align:right;">
1654.829
</td>
<td style="text-align:right;">
0.5714286
</td>
<td style="text-align:right;">
0.2105263
</td>
<td style="text-align:right;">
0.2666667
</td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

### 4. Feature Engineering

Before starting any modeling, there are some data process and feature
engineering steps to take on:

#### Feature selection with Random Forest

There are a lot of variables collected in the match report that are
likely not predictive of a matches outcome. To remove those from the
dataset, a random forest is used to determine which variables are
relatively unimportant. Ultimately, I drop information about penalties,
free kick shots off target, shots on target from free kicks, and
information about shots taken from set pieces. In contrast, it appears
that the number of shots within the penalty box, total shots on target,
and overall numbers of attacks are the most predictive of match outcome.

![](SerieA_Blog_Post_files/figure-markdown_github/Feature%20Selection%20using%20Random%20Forest-1.png)

#### Feature Extraction with PCA

Even after removing 10 features from the dataset, there are still a
large number of predictors for each match’s outcome many of which are
strongly correlated. For example total shots from the home team
(shots\_h) are strongly correlated with shots on target from the home
team (shots\_on\_h), shots off target from the home team
(shots\_off\_h), home scoring chances (scoring\_chances\_h), and saves
made by the away team (saves\_a). The correlation matrix below shows
these correlations with unlagged data.

![](SerieA_Blog_Post_files/figure-markdown_github/correlation%20of%20raw%20data-1.png)

To reduce the number of features while maximizing the amount of
variation still explained, principal components analysis (PCA) was
applied as a [pre-processing
technique](https://topepo.github.io/caret/pre-processing.html#transforming-predictors)
in caret. PCA may remove interpretability of the models, but it will
also help reduce the number of explanatory variables and help avoid
over-fitting.

------------------------------------------------------------------------

### 5. Distribution of Outcomes by Team

It’s worthwhile to point out that the distribution of outcomes is
naturally different by team. Dominant teams like Juventus, Napoli, Roma,
and Inter Milan all have win percentages over 50%. This class imbalance
has consequences for the models built. However, for the example below,
we’ll focus on Sampdoria which has a relatively balanced distribution of
outcomes for seasons 2015-16 - 2018-19: 34.8% Win, 23.6%, Loss 41.4%.

![](SerieA_Blog_Post_files/figure-markdown_github/outcome_viz-1.png)

### 6. Illustrative Example with U.C Sampdoria

The records for Sampdoria have been broken apart into two datasets:
`Samp_train` with records from the “2015-16”,“2016-17”,“2017-18”, and
“2018-19” seasons and `Samp_test` which has records from the ‘2019-20’
season. Each dataset has been scrubbed of the first three records from
each season as they do not have lagged average values for the various
features. Various models will be trained on the `Samp_train` set and
then tested individually and in an ensemble on the `Samp_test` set.

Before modeling, caret’s *trainControl* function can be used to set up
the training dataset in the same way for each model. In this case, the
training dataset is being partitioned using the timeslice function so
that the initial training data is made up of 10 observations
(initialWindow) to predict the next match (horizon=1). This process is
continued for each prediction with all historical data being used
(fixedWindow=FALSE). **Note** that in this post, none of the models have
had their hyperparameters tuned.

``` r
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 10,
                              horizon = 1,
                              fixedWindow = FALSE,
                              summaryFunction = mnLogLoss,
                              classProbs = TRUE)
```

#### Evaluation Metrics

Each of the models will be measured by the same evaluation metrics:

-   **Accuracy** *(TruePositives+TrueNegatives/N)* The number of correct
    predictions divided by total number of predictions

-   **Precision** *(TruePositives/(TruePositives + FalsePositives))* The
    true positive rate class (e.g. Of the number of draws predicted, the
    proption of correctly predicted draws)

-   **Recall/Sensitivity** *(TruePositives/(TruePositives +
    FalseNegatives))* The true negative rate by class (e.g. Of the
    number of draws in the test dataset, the number of correctly
    predicted draws).

#### Multinomial Logistic Regression

The multinomial logistic regression approach is done using the ‘nnet’
package. Below you can see the summary of the model run, the confusion
matrix is printed for a quick evaluation of this model.

-   **Accuracy** of the model is 42.86%

-   **Precision** among the classes is 25.0% for draws, 66.7% for
    losses, and 42.8% for wins

-   **Recall/Sensitivity** was 40.0% for draws, 40.0% for losses, and
    50.0% for wins.

<!-- -->

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction D L W
    ##          D 2 3 3
    ##          L 2 4 0
    ##          W 1 3 3
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.4286          
    ##                  95% CI : (0.2182, 0.6598)
    ##     No Information Rate : 0.4762          
    ##     P-Value [Acc > NIR] : 0.7427          
    ##                                           
    ##                   Kappa : 0.1572          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.2407          
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: D Class: L Class: W
    ## Precision             0.25000   0.6667   0.4286
    ## Recall                0.40000   0.4000   0.5000
    ## F1                    0.30769   0.5000   0.4615
    ## Prevalence            0.23810   0.4762   0.2857
    ## Detection Rate        0.09524   0.1905   0.1429
    ## Detection Prevalence  0.38095   0.2857   0.3333
    ## Balanced Accuracy     0.51250   0.6091   0.6167

#### SVM

The second model tried out is a support vector machine from the
‘kernlab’ package.

-   Notably, the model predicts losses for over 85% of the matches in
    the test set
-   Training accuracy was 54.2%
-   The overall test accuracy of the model is 47.62%
-   Precision among the classes is 0% for draws, 50.0% for losses, and
    50.0% for wins.
-   Recall is 0% for draws, 90% for losses, and 16.7% for wins.

<!-- -->

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction D L W
    ##          D 0 0 2
    ##          L 3 7 3
    ##          W 2 3 1
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.381           
    ##                  95% CI : (0.1811, 0.6156)
    ##     No Information Rate : 0.4762          
    ##     P-Value [Acc > NIR] : 0.8629          
    ##                                           
    ##                   Kappa : -0.0302         
    ##                                           
    ##  Mcnemar's Test P-Value : 0.3916          
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: D Class: L Class: W
    ## Precision             0.00000   0.5385  0.16667
    ## Recall                0.00000   0.7000  0.16667
    ## F1                        NaN   0.6087  0.16667
    ## Prevalence            0.23810   0.4762  0.28571
    ## Detection Rate        0.00000   0.3333  0.04762
    ## Detection Prevalence  0.09524   0.6190  0.28571
    ## Balanced Accuracy     0.43750   0.5773  0.41667

#### C5.0

The C5.0 is tree-based algorithm which produces the highest overall
accuracy of all the models tested thus far.

-   In general, the random forest over-estimates wins, but has an
    overall test accuracy of 61.9%.
-   Precision among the classes is 66.7% for draws, 85.7% for losses,
    and 45.4% for wins.
-   Recall is 40.0% for draws, 60.0% for losses, and 83.3% for wins.

<!-- -->

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction D L W
    ##          D 2 0 1
    ##          L 1 6 0
    ##          W 2 4 5
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.619           
    ##                  95% CI : (0.3844, 0.8189)
    ##     No Information Rate : 0.4762          
    ##     P-Value [Acc > NIR] : 0.1374          
    ##                                           
    ##                   Kappa : 0.4207          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.1490          
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: D Class: L Class: W
    ## Precision             0.66667   0.8571   0.4545
    ## Recall                0.40000   0.6000   0.8333
    ## F1                    0.50000   0.7059   0.5882
    ## Prevalence            0.23810   0.4762   0.2857
    ## Detection Rate        0.09524   0.2857   0.2381
    ## Detection Prevalence  0.14286   0.3333   0.5238
    ## Balanced Accuracy     0.66875   0.7545   0.7167

------------------------------------------------------------------------

#### Ensemble Method

In statistics and machine learning, ensemble methods leverage multiple
machine learning models to obtain a single set of predictions informed
by all the original models. Essentially, each model gets to “vote” on
the outcome and the majority or plurality outcome is the winner.

Whiel the package ‘caretEnsemble’ supports adding a ensemble method to
the end of the modeling pipeline, it doesn’t support the timeslice
feature used above, so a custom, basic approach is used where each
model’s predictions are weighted by that model’s overall stest accuracy.

Below you can see the results from 21 matches from Sampdoria’s 2019-20
season, specifically rounds 4-24. In total 13 of those matches were
correctly predicted.

    ##    match_date      Team          Opp actual prediction Accuracy     D     L
    ## 1  2019-09-22 Sampdoria       Torino      W          W        1 0.080 0.297
    ## 2  2019-09-25 Sampdoria   Fiorentina      L          W        0 0.274 0.235
    ## 3  2019-09-28 Sampdoria        Inter      L          W        0 0.121 0.384
    ## 4  2019-10-05 Sampdoria HellasVerona      L          L        1 0.345 0.483
    ## 5  2019-10-20 Sampdoria         Roma      D          L        0 0.218 0.558
    ## 6  2019-10-27 Sampdoria      Bologna      L          L        1 0.182 0.536
    ## 7  2019-10-30 Sampdoria        Lecce      D          D        1 0.433 0.171
    ## 8  2019-11-04 Sampdoria         Spal      W          W        1 0.391 0.137
    ## 9  2019-11-10 Sampdoria     Atalanta      D          W        0 0.099 0.424
    ## 10 2019-11-24 Sampdoria      Udinese      W          W        1 0.177 0.159
    ## 11 2019-12-02 Sampdoria     Cagliari      L          L        1 0.067 0.794
    ## 12 2019-12-08 Sampdoria        Parma      L          W        0 0.136 0.171
    ## 13 2019-12-14 Sampdoria        Genoa      W          D        0 0.666 0.121
    ## 14 2019-12-18 Sampdoria     Juventus      L          L        1 0.058 0.837
    ## 15 2020-01-06 Sampdoria        Milan      D          W        0 0.212 0.390
    ## 16 2020-01-12 Sampdoria      Brescia      W          W        1 0.277 0.192
    ## 17 2020-01-18 Sampdoria        Lazio      L          L        1 0.152 0.703
    ## 18 2020-01-26 Sampdoria     Sassuolo      D          D        1 0.469 0.253
    ## 19 2020-02-03 Sampdoria       Napoli      L          L        1 0.227 0.537
    ## 20 2020-02-08 Sampdoria       Torino      W          W        1 0.303 0.234
    ## 21 2020-02-16 Sampdoria   Fiorentina      L          W        0 0.283 0.146
    ##        W
    ## 1  0.622
    ## 2  0.491
    ## 3  0.495
    ## 4  0.172
    ## 5  0.224
    ## 6  0.282
    ## 7  0.396
    ## 8  0.472
    ## 9  0.476
    ## 10 0.665
    ## 11 0.139
    ## 12 0.693
    ## 13 0.213
    ## 14 0.105
    ## 15 0.398
    ## 16 0.531
    ## 17 0.145
    ## 18 0.279
    ## 19 0.236
    ## 20 0.463
    ## 21 0.572

------------------------------------------------------------------------

#### Results

So how much money could have theoretically been made on those matches?
Using historical betting odds from Bet365, we can see what payouts would
have been earned at dependent upon how much faith was put in the
ensemble model. For example, I may only want to place a bet on an
outcome if the most likely outcome predicted by my ensemble has a
probability greater than 0.55.

Using the table above, I attach the ensemble payouts for each match if
the outcome with the highest probability was bet on.

Next I iterate through different minimum probabilities required to place
a bet (cut\_off) to see how that would affect the number of matches bet
upon and the total return.

    ##    cut_off num_bets return profit
    ## 1     0.50       12  11.45  -0.55
    ## 2     0.51       12  11.45  -0.55
    ## 3     0.52       12  11.45  -0.55
    ## 4     0.53       12  11.45  -0.55
    ## 5     0.54        9   5.80  -3.20
    ## 6     0.55        9   5.80  -3.20
    ## 7     0.56        8   6.80  -1.20
    ## 8     0.57        8   6.80  -1.20
    ## 9     0.58        7   7.80   0.80
    ## 10    0.59        7   7.80   0.80
    ## 11    0.60        7   7.80   0.80
    ## 12    0.61        7   7.80   0.80
    ## 13    0.62        7   7.80   0.80
    ## 14    0.63        6   5.20  -0.80
    ## 15    0.64        6   5.20  -0.80
    ## 16    0.65        6   5.20  -0.80
    ## 17    0.66        6   5.20  -0.80
    ## 18    0.67        4   4.05   0.05
    ## 19    0.68        4   4.05   0.05
    ## 20    0.69        4   4.05   0.05
    ## 21    0.70        3   5.05   2.05
    ## 22    0.71        2   3.65   1.65
    ## 23    0.72        2   3.65   1.65
    ## 24    0.73        2   3.65   1.65
    ## 25    0.74        2   3.65   1.65
    ## 26    0.75        2   3.65   1.65

![](SerieA_Blog_Post_files/figure-markdown_github/Samp%20B365-1.png)

You might expect this char to look different. You might expect the
fewer, higher likelihood bets made, the more profit to be returned and
for this line to always trend upwards. However…

------------------------------------------------------------------------

### 7. Ensemble Accuracy For Other Teams

Running the same approach on all teams who have been in Serie A for each
of the five recorded seasons, there is a great variety in the amount of
accuracy of the ensemble method by team:

<table>
<thead>
<tr>
<th style="text-align:left;">
Team
</th>
<th style="text-align:right;">
Ensemble.Accuracy
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Bologna
</td>
<td style="text-align:right;">
0.2727273
</td>
</tr>
<tr>
<td style="text-align:left;">
Milan
</td>
<td style="text-align:right;">
0.3043478
</td>
</tr>
<tr>
<td style="text-align:left;">
Fiorentina
</td>
<td style="text-align:right;">
0.3181818
</td>
</tr>
<tr>
<td style="text-align:left;">
Torino
</td>
<td style="text-align:right;">
0.3636364
</td>
</tr>
<tr>
<td style="text-align:left;">
Sassuolo
</td>
<td style="text-align:right;">
0.3809524
</td>
</tr>
<tr>
<td style="text-align:left;">
Roma
</td>
<td style="text-align:right;">
0.5000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Udinese
</td>
<td style="text-align:right;">
0.5000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Genoa
</td>
<td style="text-align:right;">
0.5000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Inter
</td>
<td style="text-align:right;">
0.5238095
</td>
</tr>
<tr>
<td style="text-align:left;">
Atalanta
</td>
<td style="text-align:right;">
0.5238095
</td>
</tr>
<tr>
<td style="text-align:left;">
Lazio
</td>
<td style="text-align:right;">
0.5909091
</td>
</tr>
<tr>
<td style="text-align:left;">
Sampdoria
</td>
<td style="text-align:right;">
0.6190476
</td>
</tr>
<tr>
<td style="text-align:left;">
Juventus
</td>
<td style="text-align:right;">
0.7727273
</td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

### 8. Considering Betting on Draws

Soccer is different from every other popular sport in that it allows for
draws. For the casual sports fan who is drawn to watching sports to see
two teams compete and a winner declared, this can seem incredibly
boring.

This mentality is reflected in the odds offered by odds makers who
consistently offer slightly better odds on draws, because casual punters
are more likely to bet on one of the two teams winning. This is
reflected in the historical odds data for Serie A. Draws have a minimum
odds of 2.4, over twice the minimum of either home or away outcomes.
Average returns are highest for an away win (4.86), then a draw (4.06),
and last a home win (2.86). This is unsurprising if we consider how home
advantage affects matches.

![](SerieA_Blog_Post_files/figure-markdown_github/Betting%20on%20Draws-1.png)

A similarly striking pattern on returns is observed when filtering for
odds that actually paid out. The mean payout among draws is now higher
than away wins and the minimum payout is greater than twice that of away
or home wins.

![](SerieA_Blog_Post_files/figure-markdown_github/Betting%20on%20Draws%202-1.png)

Given the higher payout of draws on average, it may make sense to
re-cast the multi-nomial classification problem (Win, Loss, Draw) to a
binomial classification problem with a focus on identifying draws
(identifying non-draws wouldn’t be helpful for betting purposes).

------------------------------------------------------------------------

### 9. Conclusion and Next Steps

-   Expand this approach to all the other teams in the dataset and
    account for class imbalance through over or under sampling
    -   Investigate other modeling approaches. Caret has over [238
        models](https://topepo.github.io/caret/train-models-by-tag.html)
        that can be incorporated along with all their tuning parameters.
    -   Gather more data: xG, player-level data, etc.
