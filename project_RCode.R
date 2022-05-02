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


# Matrix Scatterplot (Without 3 High Leverage Points)
lotsize_minus_high_lev <- realestate %>%
  filter(LotSize != 416869 & LotSize != 108900 & LotSize != 84071)

n2 <- 141
numtrain2 = ceiling(.8*n)
set.seed(678)
train_ind2 = sample(n, numtrain)
traindata2 = lotsize_minus_high_lev[train_ind, ]
testdata2 = dataframe[-train_ind, ]
set.seed(NULL)

pairs(SoldPrice ~ Sqft + Bed + Bath + YearBuilt + LotSize + Parking, data = traindata2, 
      col = c('aquamarine', 'cornflowerblue','royalblue', 'cadetblue', 'cyan', 
              'darkblue', 'deepskyblue', 'darkseagreen', 'darkseagreen1')[factor(traindata$HomeType)], 
      pch = 19)


# Interaction Plot 
interaction = aov(SoldPrice ~ Sqft*HomeType, data = traindata)
interaction.plot(x.factor = traindata$Sqft, #x-axis variable
                 trace.factor = traindata$HomeType, #variable for lines
                 response = traindata$SoldPrice, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Sold Price",
                 xlab = "Square Footage (sqft)",
                 col = c('aquamarine', 'cornflowerblue','royalblue', 'cadetblue', 'cyan', 
                         'darkblue', 'deepskyblue', 'darkseagreen', 'darkseagreen1'),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Home Type")


# Scatterplot of Sold Price and Sqft, Colored By Home Type
traindata %>%
  ggplot(aes(y = SoldPrice, x = Sqft, color = HomeType)) +
  geom_point()



# Histograms of Quantitative Variables 
traindata %>%
  ggplot(aes(x = Sqft, fill = HomeType)) +
  geom_histogram()

traindata %>%
  ggplot(aes(x = YearBuilt, fill = Pool)) +
  geom_histogram(binwidth = 10)

traindata2 %>%
  ggplot(aes(x = LotSize, fill = HomeType)) +
  geom_histogram(binwidth = 2000)




