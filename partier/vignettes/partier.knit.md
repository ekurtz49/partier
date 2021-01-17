---
title: "partier"
author: "Emily Kurtz"
date: "December 2020"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{partier}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

## Purpose of Package

The partier package was designed with one goal in mind: to offer a tool to analyze party policy positions via the L1, or cityblock, metric. According to Benoit and Laver (2005), the cityblock metric performs better than the Euclidean metric in psychological, political choice contexts (p 32). The L1 norm's superiority in evaluating individuals' political choices is especially evident when the underlying dimensions being analyzed are separable, meaning that one dimension can be evaluated more or less independently of another (p 35). On the other hand, the Euclidean metric performs better when the dimensions are integral, or relatively dependent, though in these cases the best option may be to reduce the dimensionality by condensing all integral dimensions down to one, where the L1 and Euclidean metrics become identical. Despite the cityblock metric's superior predictive power and modelling fit though, political scientists more often use the Euclidean metric because, as Benoit and Laver found, it is "tractable given current analytical techniques" (p 32). Thus, this package seeks out a way to make the cityblock metric more "tractable."

The partier package provides tools for calcuating party "territories," or set of possible policy positions for which that party's own position is closer than any other party's position, given a user-specified matrix of parties' policy positions. Any number of parties can be analyzed, as well as any number of policy dimensions, though of course larger numbers on both fronts requires more computational time. The package also includes one dataset, democracies, which offers clean example data with which to get accustomed to the package's functions. Finally, the package includes some voter simulation functions which generates a set of voters' opinions on the dimensions of interests based on the user's inputted underlying distributions of the policy dimensions.

The bulk of the theoretical inspiration for this package comes from Benoit and Laver's 2005 book Party Policy in Modern Democracies. One thing to note is that this package does not seek to determine parties' best strategic positions or encourage any policy tweaking for the purposes of winning more elections. For a multitude of reasons, such a goal is very optimistic, and thus the scope of this package is limited to visualization and summarization and offering a theoretical situation to which empirical data can be compared.

## Installation and Use

Save the partier file to your working directory and repeat the below lines of code (uncomment the first line) to access and use the partier package.


```r
# install('partier')
library(partier)
```

An older version of the partier file is available at https://github.umn.edu/kurtz217/partier, and the updated package will soon be available there and through CRAN.

## Functions

The partier package contains a number of functions, many of which are simple helper functions that, while not intended for use in typical workflows, can be useful in a number of contexts extending beyond visual models of party competition. At the moment, the package includes the following main functions:

coordinate_distances

plot.PartyMatrix

summary.PartyMatrix

sim_vote

sim_voter_positions

The following helper functions are also included:

cityblock

duplicate_minima

plot1d

plot2d

plot_matrix

More information for all functions can be found in the help text (accessed, among other ways, using the help function or preceding the function name with a question mark, i.e. "help(plot2d)" or "?plot2d").

## Dataset - democracies

This data set is an edited version of data collected by Kenneth Benoit and Michael Laver and presented in their book Party Policy in Modern Democracies (2005). A complete description of all variables and items from the original data set can be found at the following site: https://www.tcd.ie/Political_Science/ppmd/PPMD_summary_data_GUIDE.pdf.

The democracies data set is just a subset of the original data. Certain factors thought to be less useful to the main intent of the package have been removed in order to make the data set easier to use when learning the package. The version included in this package contains all rows of the original set, but only four columns: Country, Party, Dimension, and Mean. There are 47 countries represented, and a total of 100 parties in these 47 countries. These parties are rated on a scale from 1 to 20 on some number of policy dimensions. There are 38 total policy dimensions, but some, such as "Quebec," which measures the party's support of Quebec sovereignty, are country-specific. The Quebec dimension, for example, would only be evaluated for Canadian parties. The parties are rated by multiple political experts, and their average score for each dimension for which they are rated is recorded in the Mean column. Of course, both the choice of dimensions and the experts' opinions for the parties' rankings on these dimensions are free to be disputed, but this is not the goal of the package. That said, the accompanying paper dedicates some time to this discussion.

More information on the data set can be found in the help text.


```r
# help(democracies)
```

## Problem Solving Example - Analyzing Party Territories

Below we illustrate the steps one could go through to get a rough sense of how a country's political parties compare on a number of dimensions of interest and what types of theoretical voters these parties would attract. In general, each process should start with creating a PartyMatrix object using the coordinate_distances function. That object can then be used to create summaries and plots in order to more concisely understand and visualize the parties positions on the policies of interest.

In our example, we will analyze five main parties in 2005 German politics - a Green, Social Democratic, Liberal, Conservative, and Far Right party. These parties' ideological platforms represent a wide variety of policy views present in many western democracies (Ennser, 2012).

We start by gathering the parties' views on taxes and spending, social issues, and the environment, and running coordinate_distances on those values.


### coordinate_distances

The coordinate_distances function will typically be the first function used in this workflow. It takes in a matrix of parties' positions on all policies of interest and returns the "winning" party for each theoretical position a voter could have on all policies. 

An important note to reiterate when using coordinate_distances, as is also mentioned in the help text for the function, is to have parties represented by columns and policies represented by rows. In our case, as we will see, all parties' views on taxes are represented in row 1, on social issues in row 2, and on the environment in row 3. Likewise, the conservative CDU party's views are all in column 1, the liberal FDP is represented by column 2, the green GRU column 3, the far right NDP column 4, and the social democratic SPD column 5. We name the matrix we create as input for the coordinate_distances function "policies," and this means that, for example, the second row and third column of the policies matrix represents GRU's social policies dimension position.


```r
# get subset of dataset for German parties
germany <- democracies[democracies$Country == "DE", ]

# create a 'policies' matrix to be used in coordinate_distances
policies <- matrix(c(germany[1, 4], germany[2, 4], germany[3, 4], germany[31, 4], 
    germany[32, 4], germany[33, 4], germany[41, 4], germany[42, 4], germany[43, 4], 
    germany[51, 4], germany[52, 4], germany[53, 4], germany[81, 4], germany[82, 4], 
    germany[83, 4]), nrow = 3)

# create PartyMatrix object - german - for future work
german <- coordinate_distances(policies = policies, scale.upper = 20)
```

The last row in the above code chunk creates a PartyMatrix object using the coordinate_distances function, which we name "german." Let's see what german looks like.


```r
# get first six, and last six, rows of the german PartyMatrix matrix
head(german$matrix)
```

```
##      Dimension 1 Dimension 2 Dimension 3 Distance from Party 1
## [1,]           1           1           1              41.82553
## [2,]           2           1           1              40.82553
## [3,]           3           1           1              39.82553
## [4,]           4           1           1              38.82553
## [5,]           5           1           1              37.82553
## [6,]           6           1           1              36.82553
##      Distance from Party 2 Distance from Party 3 Distance from Party 4
## [1,]              37.75841             13.578513              39.63497
## [2,]              36.75841             12.578513              38.63497
## [3,]              35.75841             11.578513              37.63497
## [4,]              34.75841             10.578513              36.63497
## [5,]              33.75841              9.578513              35.63497
## [6,]              32.75841              8.578513              34.63497
##      Distance from Party 5 Closest Party
## [1,]              24.52292             3
## [2,]              23.52292             3
## [3,]              22.52292             3
## [4,]              21.52292             3
## [5,]              20.52292             3
## [6,]              19.52292             3
```

```r
tail(german$matrix)
```

```
##         Dimension 1 Dimension 2 Dimension 3 Distance from Party 1
## [7995,]          15          20          20              10.17447
## [7996,]          16          20          20              11.17447
## [7997,]          17          20          20              12.17447
## [7998,]          18          20          20              13.17447
## [7999,]          19          20          20              14.17447
## [8000,]          20          20          20              15.17447
##         Distance from Party 2 Distance from Party 3 Distance from Party 4
## [7995,]              21.66712              38.42149              12.36503
## [7996,]              20.66712              39.42149              13.36503
## [7997,]              19.66712              40.42149              14.36503
## [7998,]              18.66712              41.42149              15.36503
## [7999,]              18.24159              42.42149              16.36503
## [8000,]              19.24159              43.42149              17.36503
##         Distance from Party 5 Closest Party
## [7995,]              27.47708             1
## [7996,]              28.47708             1
## [7997,]              29.47708             1
## [7998,]              30.47708             1
## [7999,]              31.47708             1
## [8000,]              32.47708             1
```

```r
# get the dimensions of the PartyMatrix matrix
dim(german$matrix)
```

```
## [1] 8000    9
```

```r
# see what is created by the coordinate_distances function
class(german)
```

```
## [1] "PartyMatrix" "list"
```

First, note that the function indeed creates a PartyMatrix object (as shown by the class function's output), in order to allow the package's summary and plot functions to be accessed via their generic versions. Also note that the function creates a matrix with 8000 rows and 9 columns. There is one row for each possible ordered tuple. The three policies are measured from 1 to 20, and thus, there are $20^3 = 8000$ possible tuples. The tuples are recorded in the first columns, in as many columns as there are policies, so three in this case. The remaining columns, barring the last, contain the distances from the given policy positions (represented by the tuples) and the party's (that which corresponds to the column we are in) platform. For example, let's look at column 4, which corresponds to the first party (CDU). Rounded, this party's views are, in order, 14, 16, and 15. Thus, this party would be approximately $(14-1)+(16-1)+(15-1) = 42$ units away from the ordered tuple (1,1,1), i.e. the policy dimensions represented in the first row. We indeed see the value 41.82553, about 42, in the first row, fourth column.


```r
c(germany[1, 4], germany[2, 4], germany[3, 4])
```

```
## [1] 14.40000 15.91489 14.51064
```

Indeed, looking at row 1 of german, this is what we see. Columns 5 through 8 likewise represent the other parties' distances in the same way.

Finally, the last column represents the party with the minimum distance to the given ordered tuple. Again in row 1, column 6, which represents party 3, has the minimum value of all party columns, and so a 3 appears in the last column. If two parties tie for the minimum distance, a 0 is recorded.

### summary.PartyMatrix

A basic question we might ask is which parties have the largest potential voter bases, i.e. which parties have the most ordered tuples which would, theoretically (if they're voting for the closest party), prefer them over other parties. The territory_totals portion of the generic summary function gives us this information.


```r
summary(german)
```

```
## $distances
##  Distance from Party 1 Distance from Party 2 Distance from Party 3
##  Min.   : 0.9745       Min.   : 0.8222       Min.   : 0.5785      
##  1st Qu.:12.8255       1st Qu.:15.6671       1st Qu.:15.1583      
##  Median :17.8255       Median :21.1778       Median :20.8004      
##  Mean   :18.0217       Mean   :21.6782       Mean   :20.9747      
##  3rd Qu.:22.8255       3rd Qu.:27.7584       3rd Qu.:26.4627      
##  Max.   :41.8255       Max.   :48.1778       Max.   :44.4627      
##  Distance from Party 4 Distance from Party 5
##  Min.   : 0.365        Min.   : 0.6492      
##  1st Qu.:13.832        1st Qu.:11.5229      
##  Median :19.635        Median :15.4771      
##  Mean   :19.515        Mean   :15.5888      
##  3rd Qu.:24.635        3rd Qu.:19.3508      
##  Max.   :42.635        Max.   :33.3508      
## 
## $territory_totals
##    1    2    3    4    5 
## 1565  985 1506 1573 2371
```

Above, we see that Pary 5. SPD, has the largest number, which is one of the more successful parties. While these totals only represent the parties on three dimensions, and while other aspects of a party factor into a voter's decision, these numbers give some idea of which parties have the potential for success. That said, a major drawback (one that this part of the workflow doesn't necessarily attempt to fix, though can be somewhat addressed by the voter simulation functions) to this information is that it tells us nothing about the underlying distribution of voters. While the SPD can claim almost 30% of all possible tuples as its "territory," this does not mean 30% of voters are closest to that party. Such a conclusion would assume that voters are uniformly distributed about the tuples.

The summary function also gives descriptive statistics for how far away a party's likely voters' policy opinions lie from the party's platform overall. While this package again does not seek to encourage policy movement, this information could be helpful, for example, in comparing party registration and turnout numbers, as one party may have a larger territory but more voters who are substantially farther away, and thus less likely to support their closest party.

### plot.PartyMatrix

This generic function works on any number of policy dimensions, as long as R doesn't throw a plotting error due to the user having too small of a screen to render the necessary pixels. On an average sized, desktop computer monitor, R seems to handle plotting 5-6 dimensions reasonably well.

There are three different types of visuals, which are themselves directly created by the helper functions plot1d, plot2d, and plot_matrix. All of these plots are best created by the user through the plot function, though. If the user plots one dimension, the plot1d function creates a one-dimensional plot showing the parties' territories on that one dimension. If the user plots two dimensions, the plot2d function creates a two-dimensional plot, with one dimension on each the x and y axis, showing the parties' terriroties on those two dimensions, calculated, of course, using the L1 norm. If the user plots three or more dimensions, a plot matrix is created by the plot_matrix function. This function utilizes both of the previous plot types, with one-dimensional plots on the main diagonal and two-dimensional plots elsewhere. Exclusing main diagonal plots which only have one axis, the plots are organized in the matrix so that the dimension appearing on the x axis shows up in its corresponding column and the dimension appearing on the y axis shows up in its corresponding row. For example, if we plot three dimensions and look at the plot in the second row and third column, the plot will have Dimension 3 on the x axis and Dimension 2 on the y axis. This implies that the matrix is, effectively, reflexive across the main diagonal; the plot in the $i^th$ row and $j^th$ column conveys exactly the same information as the plot in the $j^th$ row and $i^th$ column.

Keeping in mind that our example of interest contains five parties and three dimensions, let's see what happens if we plot our PartyMatrix object:





















