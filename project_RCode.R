library(tidyverse)
library(readxl)
library(scatterplot3d)
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


# VI. Variable Pre-Processing

# Matrix Scatterplot (Without 3 High Leverage Points)
lotsize_minus_high_lev <- realestate %>%
  filter(LotSize != 416869 & LotSize != 108900 & LotSize != 84071)

n2 <- 141
numtrain2 = ceiling(.8*n2)
set.seed(678)
train_ind2 = sample(n2, numtrain2)
traindata2 = lotsize_minus_high_lev[train_ind2, ]
testdata2 = dataframe[-train_ind2, ]
set.seed(NULL)

pairs(SoldPrice ~ Sqft + Bed + Bath + YearBuilt + LotSize + Parking, data = traindata2, 
      col = c('aquamarine', 'cornflowerblue','royalblue', 'cadetblue', 'cyan', 
              'darkblue', 'deepskyblue', 'darkseagreen', 'darkseagreen1')[factor(traindata$HomeType)], 
      pch = 19)

# Correlation Matrix 
cor(traindata[,c(1, 2, 3, 4, 5, 6, 7)])
cor(traindata2[,c(1, 2, 3, 4, 5, 6, 7)])

# Interaction Plot Between Sold Price and Square Footage (Home Type)
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
  geom_histogram() +
  scale_fill_manual(values = c('aquamarine', 'cornflowerblue','royalblue', 'cadetblue', 'cyan', 
                               'darkblue', 'deepskyblue', 'darkseagreen', 'darkseagreen1'))

traindata %>%
  ggplot(aes(x = YearBuilt, fill = Pool)) +
  geom_histogram(binwidth = 10) +
  scale_fill_manual(values = c('royalblue', 'cyan'))

traindata2 %>%
  ggplot(aes(x = LotSize, fill = HomeType)) +
  geom_histogram(binwidth = 2000) +
  scale_fill_manual(values = c('aquamarine', 'cornflowerblue','royalblue', 'cadetblue', 'cyan', 
                               'darkblue', 'deepskyblue', 'darkseagreen', 'darkseagreen1'))


# Interaction Plot Between TWO CATEGORICAL VARIABLES 


# 3D Scatterplot 
s3d <- scatterplot3d(traindata2$Sqft, y=traindata2$SoldPrice, z=traindata2$LotSize,
                     xlab = "Square Footage (sqft)",
                     ylab = "Sold Price (Dollars)",
                     zlab = "Lot Size (sqft)", 
                     angle=55, pch = 16)
# Add regression plane
my.lm <- lm(traindata2$Sqft ~ traindata2$SoldPrice + traindata2$LotSize)
s3d$plane3d(my.lm)



