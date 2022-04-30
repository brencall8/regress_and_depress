library(tidyverse)
realestate <- read_csv("Real_Estate.csv")

# IV. Split the Data
n <- 522
numtrain = ceiling(.8*n)
set.seed(678)
train_ind = sample(n, numtrain)
traindata = realestate[train_ind, ]
testdata = dataframe[-train_ind, ]
set.seed(NULL)


# V. Data Visualization
pairs(Price ~ Sqft + Bed + Bath + Age + Garage, data = traindata, col = c('cornflowerblue','royalblue')[factor(traindata$Quality)], pch = 19)
