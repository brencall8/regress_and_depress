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
# Matrix Scatterplot
pairs(SoldPrice ~ Sqft + Bed + Bath + YearBuilt + LotSize + Parking, data = traindata, 
      col = c('aquamarine', 'cornflowerblue','royalblue', 'cadetblue', 'cyan', 
              'darkblue', 'deepskyblue', 'darkseagreen', 'darkseagreen1')[factor(traindata$HomeType)], 
      pch = 19)


# Matrix Scatterplot (Lot Size - Without 2 High Leverage Points)
lotsize_minus_high_lev <- realestate %>%
  filter(LotSize != 416869 & LotSize != 108900 & LotSize != 84071)

n2 <- 141
numtrain2 = ceiling(.8*n)
set.seed(678)
train_ind2 = sample(n, numtrain)
traindata2 = lotsize_minus_high_lev[train_ind, ]
testdata2 = dataframe[-train_ind, ]
set.seed(NULL)

pairs(SoldPrice ~ LotSize, data = traindata2, 
      col = c('aquamarine', 'cornflowerblue','royalblue', 'cadetblue', 'cyan', 
              'darkblue', 'deepskyblue', 'darkseagreen', 'darkseagreen1')[factor(traindata$HomeType)], 
      pch = 19)


# Interaction Plot 
interaction = aov(SoldPrice ~ Sqft*HomeType, data = realestate)
interaction.plot(x.factor = realestate$Sqft, #x-axis variable
                 trace.factor = realestate$HomeType, #variable for lines
                 response = realestate$SoldPrice, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Sold Price",
                 xlab = "Square Footage (sqft)",
                 col = c('aquamarine', 'cornflowerblue','royalblue', 'cadetblue', 'cyan', 
                         'darkblue', 'deepskyblue', 'darkseagreen', 'darkseagreen1'),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Home Type")


# Scatterplot 
realestate %>%
  ggplot(aes(y = SoldPrice, x = Sqft, color = HomeType)) +
  geom_point()
