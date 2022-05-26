library(tidyverse)
library(readxl)
library(scatterplot3d)
library(GGally)
library(car)
library('lmtest')
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


# Matrix Scatterplot with ggpairs()
ggpairs(traindata[,-c(8:12)], 
        upper=list(continuous='points'),
        lower=list(continuous='cor',combo='blank',discrete='blank'))


# Interaction Plot Between Sold Price and Square Footage (Home Type)
traindata$HomeType=factor(traindata$HomeType)
traindata <- traindata %>%
  mutate(HomeTypeNew = 
           case_when(HomeType == "Condominium" ~ "Condo", 
                     HomeType == "SingleFamilyResidence" ~ "SingleFam",
                     HomeType == "ManufacturedHome" ~ "Manufactured",
                     TRUE ~ "Other")) 
traindata$HomeTypeNew=factor(traindata$HomeTypeNew)
fit_interaction = lm(SoldPrice~Sqft*HomeTypeNew, data=traindata)
summary(fit_interaction)

plot(SoldPrice ~ Sqft, data = traindata, 
     col = c('cornflowerblue','royalblue','cyan', 'darkseagreen')[HomeTypeNew], 
     pch = c(15, 16, 17, 18))
legend('bottomright',legend=levels(traindata$HomeTypeNew), 
       pch = c(15, 16, 17, 18), 
       col = c('cornflowerblue','royalblue','cyan', 'darkseagreen'), 
       lty = 1)
coeff = fit_interaction$coeff
abline(coeff[1], coeff[2], col='cornflowerblue')
abline(coeff[1] + coeff[3], coeff[2] + coeff[6], col = 'royalblue') 
abline(coeff[1] + coeff[4], coeff[2] + coeff[7], col = 'cyan') 
abline(coeff[1] + coeff[5], coeff[2] + coeff[8], col = 'seagreen') 


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


# Interaction Plot Between Home Type and Pool
traindata$Pool=factor(traindata$Pool)
with(traindata, interaction.plot(HomeTypeNew, Pool, log(SoldPrice), type = 'b', 
                                 col = c('royalblue','cyan'), 
                                 pch = c(16, 17)))


# 3D Scatterplot 
s3d <- scatterplot3d(traindata2$Sqft, y=traindata2$SoldPrice, z=traindata2$LotSize,
                     xlab = "Square Footage (sqft)",
                     ylab = "Sold Price (Dollars)",
                     zlab = "Lot Size (sqft)", 
                     angle=55, pch = 16)
# Add regression plane
my.lm <- lm(traindata2$Sqft ~ traindata2$SoldPrice + traindata2$LotSize)
s3d$plane3d(my.lm)



# VII. Residual Analysis 
par(mfrow=c(2,2)) #plot in 2 by 2 grid   
plot(final_model)

plot(resid(final_model) ~ log(SoldPrice), data=traindata, ylab='Residuals', xlab='log(SoldPrice)')
abline(h=0,lty=2)

plot(resid(final_model) ~ YearBuilt, data=traindata, ylab='Residuals', xlab='YearBuilt')
abline(h=0,lty=2)

plot(resid(final_model) ~ HomeTypeNew, data=traindata, ylab='Residuals', xlab='HomeTypeNew')
abline(h=0,lty=2)

hist(resid(final_model), main='', xlab='Residuals')

shapiro.test(resid(final_model))
bptest(final_model)



# VIII. Fit a Linear Model 
final_model = lm(I(log(SoldPrice)) ~ I(log(Sqft)) + YearBuilt + HomeTypeNew, data = traindata)
summary(final_model)
vif(final_model)

