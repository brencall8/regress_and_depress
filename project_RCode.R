library(tidyverse)
library(readxl)
realestate <- read_excel("SLO_Real_Estate_Feb-Apr2022.xlsx")


# IV. Split the Data
n <- 144
numtrain = ceiling(.8*n)
set.seed(678)
train_ind = sample(n, numtrain)
traindata = realestate[train_ind, ]
testdata = dataframe[-train_ind, ]
set.seed(NULL)


# V. Data Visualization
pairs(SoldPrice ~ Sqft + Bed + Bath + YearBuilt + LotSize + Parking, data = traindata, 
      col = c('aquamarine', 'cornflowerblue','royalblue', 'cadetblue', 'cyan', 
              'darkblue', 'deepskyblue', 'darkseagreen', 'darkseagreen1')[factor(traindata$HomeType)], 
      pch = 19)

