## ----tidy=TRUE----------------------------------------------------------------
library(partier)

## ----tidy=TRUE----------------------------------------------------------------
#install.packages("partier")
#library(partier)

## ----tidy=TRUE----------------------------------------------------------------
#help(democracies)

## ----tidy=TRUE----------------------------------------------------------------
# get subset of dataset for German parties
germany<-democracies[democracies$Country=="DE",]

# create a "policies" matrix to be used in coordinate_distances
policies<-matrix(c(germany[1,4],germany[2,4],germany[3,4],
                          germany[31,4],germany[32,4],germany[33,4],
                          germany[41,4],germany[42,4],germany[43,4],
                          germany[51,4],germany[52,4],germany[53,4],
                          germany[81,4],germany[82,4],germany[83,4]),nrow=3)

# create PartyMatrix object - german - for future work
german<-coordinate_distances(policies=policies,scale.upper = 20)

## ----tidy=TRUE----------------------------------------------------------------
#get first six, and last six, rows of the german PartyMatrix matrix
head(german$matrix)
tail(german$matrix)
#get the dimensions of the PartyMatrix matrix
dim(german$matrix)
#see what is created by the coordinate_distances function
class(german)

## ----tidy=TRUE----------------------------------------------------------------
c(germany[1,4],germany[2,4],germany[3,4])

## ----tidy=TRUE----------------------------------------------------------------
summary(german)

