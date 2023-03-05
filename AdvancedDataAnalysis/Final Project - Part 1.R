### Final Project - Part 1------------------------
## Set work directory-----------------------------
setwd("/Users/tethys/Desktop/Advanced_Data_Analysis_R/Codes")

## Question 01: Call Data Frame------------------
#Required libraries
library("ggplot2")
library("moments")

## Question 02: Data inspection----------------
data1 <- read.csv("dataset1.csv", header = TRUE)
names(data1)
# "Price"     "Age"       "KM"        "FuelType"  "HP"        "MetColor"  "Automatic" "CC"       
# "Doors"     "Weight"   
dim(data1)
# 1325 Observations, 10 Variables
head(data1)
summary(data1)
# There is no NA data.
str(data1)
# $ Price    : int  13500 13750 13950 14950 13750 12950 16900 12950 16750 16950 ...
# $ Age      : int  23 23 24 26 30 32 27 23 24 30 ...
# $ KM       : int  46986 72937 41711 48000 38500 61000 94612 71138 25563 64359 ...
# $ FuelType : chr  "Diesel" "Diesel" "Diesel" "Diesel" ...
# $ HP       : int  90 90 90 90 90 90 90 69 110 110 ...
# $ MetColor : int  1 1 1 0 0 0 1 0 0 1 ...
# $ Automatic: int  0 0 0 0 0 0 0 0 0 0 ...
# $ CC       : int  2000 2000 2000 2000 2000 2000 2000 1900 1600 1600 ...
# $ Doors    : int  3 3 3 3 3 3 3 3 3 3 ...
# $ Weight   : int  1165 1165 1165 1165 1170 1170 1245 1105 1065 1105 ...

# First three variables: "Price", "Age" and "KM" are continuous variables and should be integers.

# "FuelType" is a character so at first we must change it to factor with three type of levels:
# "Gas", "Diesel" and "Petrol"
data1 <- as.data.frame(unclass(data1), stringsAsFactors = TRUE)
str(data1)
# The result is the new class for "FuelType": 
# FuelType : Factor w/ 3 levels "CNG","Diesel",..: 2 2 2 2 2 2 2 2 3 3 ...

# "HP" has defiend as "int". 
table(data1$HP)
# HP:    69  72  73  86  90  97 107 110 116 
# Counts 34  73   1 249  23 152  21 770   2 
# Here we have 9 types of "HP" where two types "HP = 73" and "HP = 116" are very rare in data set.
# We can remove them from our data set because of their lower statistical impact, but we don't do
# it yet. 
#Here we just make "fHP" as a categorical variable because it refers different types of powers.

data1$fHP <- as.factor(data1$HP)
# fHP      : Factor w/ 9 levels "69","72","73",..: 5 5 5 5 5 5 5 1 8 8 ...

# "MetColor" and "Automatic" are of course categorical variables then we add two variables 
# "fMetColor" and "fAutomatic" as:
data1$fMetColor <- as.factor(data1$MetColor)
# fMetColor: Factor w/ 2 levels "0","1": 2 2 2 1 1 1 2 1 1 2 ...
data1$fAutomatic <- as.factor(data1$Automatic)
# fAutomatic: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...

# The Cylinder Capacity variable "CC" is also a categorical variable w can have a look 
# to its distribution via:
table(data1$CC)
# CC:    1300 1332 1398 1400 1587 1598 1600 1800 1900 1975 2000 
# Counts: 248    2    2  149    4    2  786    3   30    1   98 
# Here we also so very data on "CC = 1332, 1398, 1587, 1598, 1800 and 1975". Without them
# we have only 5 important variables. But we keep them.
data1$fCC <- as.factor(data1$CC)
# fCC : Factor w/ 11 levels "1300","1332",..: 11 11 11 11 11 11 11 9 7 7 ...

# Doors are also categorical:
table(data1$Doors)
# Doors:  2   3   4   5 
# Counts: 2 593 130 600 
# Therefor we add another categorical variable as "fDoors" as:
data1$fDoors <- as.factor(data1$Doors)
# fDoors    : Factor w/ 4 levels "2","3","4","5": 2 2 2 2 2 2 2 2 2 2 ...

# The variable "Weight" is also a continuous variable and should remain integer.

# Here we can see the structur of data again
str(data1)
# 'data.frame':	1325 obs. of  14 variables:
# $ Price     : int  13500 13750 13950 14950 13750 12950 16900 12950 16750 16950 ...
# $ Age       : int  23 23 24 26 30 32 27 23 24 30 ...
# $ KM        : int  46986 72937 41711 48000 38500 61000 94612 71138 25563 64359 ...
# $ FuelType  : Factor w/ 3 levels "CNG","Diesel",..: 2 2 2 2 2 2 2 2 3 3 ...
# $ HP        : int  90 90 90 90 90 90 90 69 110 110 ...
# $ MetColor  : int  1 1 1 0 0 0 1 0 0 1 ...
# $ Automatic : int  0 0 0 0 0 0 0 0 0 0 ...
# $ CC        : int  2000 2000 2000 2000 2000 2000 2000 1900 1600 1600 ...
# $ Doors     : int  3 3 3 3 3 3 3 3 3 3 ...
# $ Weight    : int  1165 1165 1165 1165 1170 1170 1245 1105 1065 1105 ...
# $ fHP       : Factor w/ 9 levels "69","72","73",..: 5 5 5 5 5 5 5 1 8 8 ...
# $ fMetColor : Factor w/ 2 levels "0","1": 2 2 2 1 1 1 2 1 1 2 ...
# $ fAutomatic: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# $ fCC       : Factor w/ 11 levels "1300","1332",..: 11 11 11 11 11 11 11 9 7 7 ...
# $ fDoors    : Factor w/ 4 levels "2","3","4","5": 2 2 2 2 2 2 2 2 2 2 ...


## Question 03: Variable Statistics-----------------

# Then we have 6 categorical variables that we have seen their tables
# above, and we have only 4 continuous variables that we can show them
# below:

#Continuous variables
par(mfrow = c(2, 2))  # 2 rows and 2 columns
for (i in c(1, 2, 3, 10)) {
  hist(data1[,i], xlab = "", main = paste("Histogram of", names(data1)[i]))
}
par(mfrow=c(1,1))

# Plot: Price vs. other continuous variables
plot(data1$Age, data1$Price)
plot(data1$KM, data1$Price)
plot(data1$Weight, data1$Price)

# correlation between continuous variables
cor_table1 <- round(cor(data1[,c(1, 2, 3, 10)]),2)
cor_table1
#         Price  Age    KM Weight
# Price   1.00 -0.84 -0.52   0.29
# Age    -0.84  1.00  0.39  -0.27
# KM     -0.52  0.39  1.00   0.19
# Weight  0.29 -0.27  0.19   1.00

# Here we can find a series of valuable correlations between "Price" and "Age" and 
# between "Price" and "KM". In this case the "Price" increases when the two parameters 
# "Age" and "KM" are decreasing. Also, there could be a possible linear relation between 
# "Price" and "Weight" but not very strong relation. There is also a strong linear relation
# between "Age" and "KM" which means there could be co-linearity.

## Tables of categorical variables:
table(data1$FuelType)
# CNG Diesel Petrol 
#  17    132   1176 

table(data1$fMetColor)
# fMetColor:   0   1 
# Counts:    445 880 

table(data1$fAutomatic)
# fAutomatic: 0    1 
# Counts:  1254   71 

table(data1$fDoors)
# fDoors: 2   3   4   5 
# Counts: 2 593 130 600 

table(data1$fHP)
# fHP:    69  72  73  86  90  97 107 110 116 
# Counts: 34  73   1 249  23 152  21 770   2 

table(data1$fCC)
# fCC:   1300 1332 1398 1400 1587 1598 1600 1800 1900 1975 2000 
# Counts: 248    2    2  149    4    2  786    3   30    1   98

## HERE WE HAVE SOME TYPES OF CONTINUOUS VARIABLES AND SOME CATEGORICAL 
## VARIABLES. SOME OF CATEGORICAL VARIABLES HAS LESS REPEATATION BUT 
## WE KEEP THEM IN CATEGORIES.


## Question 04: Dividing Data Set--------------------

set.seed(123456)
train_cases <- sample(1:nrow(data1), nrow(data1) * 0.7) 
train <- data1[train_cases,]
test <- data1[- train_cases,]

summary(train)
# Price            Age              KM           FuelType         HP           MetColor     
# Min.   : 4450   Min.   :13.00   Min.   :     1   CNG   : 13   Min.   : 69.0   Min.   :0.0000  
# 1st Qu.: 8250   1st Qu.:50.00   1st Qu.: 48566   Diesel: 94   1st Qu.: 86.0   1st Qu.:0.0000  
# Median : 9500   Median :62.00   Median : 67100   Petrol:820   Median :110.0   Median :1.0000  
# Mean   : 9927   Mean   :59.23   Mean   : 73034                Mean   :100.2   Mean   :0.6613  
# 3rd Qu.:10950   3rd Qu.:71.00   3rd Qu.: 89992                3rd Qu.:110.0   3rd Qu.:1.0000  
# Max.   :16950   Max.   :80.00   Max.   :243000                Max.   :116.0   Max.   :1.0000  
# 
# Automatic             CC           Doors           Weight          fHP      fMetColor fAutomatic
# Min.   :0.00000   Min.   :1300   Min.   :2.000   Min.   :1000   110    :532   0:314     0:876     
# 1st Qu.:0.00000   1st Qu.:1400   1st Qu.:3.000   1st Qu.:1035   86     :184   1:613     1: 51     
# Median :0.00000   Median :1600   Median :4.000   Median :1065   97     :104                       
# Mean   :0.05502   Mean   :1556   Mean   :3.972   Mean   :1065   72     : 55                       
# 3rd Qu.:0.00000   3rd Qu.:1600   3rd Qu.:5.000   3rd Qu.:1076   69     : 22                       
# Max.   :1.00000   Max.   :2000   Max.   :5.000   Max.   :1615   90     : 16                       
# (Other): 14                       
# fDoors       fCC     
# 2:  1   1600   :542  
# 3:431   1300   :184  
# 4: 88   1400   :101  
# 5:407   2000   : 72  
#         1900   : 20  
#         1398   :  2  
#         (Other):  6 

summary(test)
# Price            Age              KM           FuelType         HP         MetColor     
# Min.   : 4350   Min.   :13.00   Min.   : 10210   CNG   :  4   Min.   : 69   Min.   :0.0000  
# 1st Qu.: 8250   1st Qu.:49.00   1st Qu.: 44540   Diesel: 38   1st Qu.: 90   1st Qu.:0.0000  
# Median : 9725   Median :62.00   Median : 63542   Petrol:356   Median :110   Median :1.0000  
# Mean   : 9968   Mean   :59.09   Mean   : 70718                Mean   :101   Mean   :0.6709  
# 3rd Qu.:11472   3rd Qu.:71.00   3rd Qu.: 87616                3rd Qu.:110   3rd Qu.:1.0000  
# Max.   :16950   Max.   :80.00   Max.   :207114                Max.   :110   Max.   :1.0000  
# 
# Automatic             CC           Doors           Weight          fHP      fMetColor fAutomatic
# Min.   :0.00000   Min.   :1300   Min.   :2.000   Min.   :1000   110    :238   0:131     0:378     
# 1st Qu.:0.00000   1st Qu.:1400   1st Qu.:3.000   1st Qu.:1035   86     : 65   1:267     1: 20     
# Median :0.00000   Median :1600   Median :4.000   Median :1060   97     : 48                       
# Mean   :0.05025   Mean   :1562   Mean   :4.073   Mean   :1064   72     : 18                       
# 3rd Qu.:0.00000   3rd Qu.:1600   3rd Qu.:5.000   3rd Qu.:1080   69     : 12                       
# Max.   :1.00000   Max.   :2000   Max.   :5.000   Max.   :1255   107    :  9                       
# (Other):  8                       
# fDoors       fCC     
# 2:  1   1600   :244  
# 3:162   1300   : 64  
# 4: 42   1400   : 48  
# 5:193   2000   : 26  
#         1900   : 10  
#         1587   :  2  
#         (Other):  4 



## Question 05: First Reg., Single variable linear regression--------------

m1 <- lm(Price ~ KM, data = train)
m1
summary(m1)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5414.1 -1345.9  -320.3  1148.3  8269.7 
mean(m1$residuals)
# -9.173436e-14
## THERE IS A LARGE VALUE OF DIFFERENCE BETWEEN MEAN AND MEDIAN 

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  1.236e+04  1.498e+02   82.56   <2e-16 *** (P-VALUE IS ACCEPTED)
#  KM          -3.337e-02  1.839e-03  -18.14   <2e-16 *** (P-VALUE IS ACCEPTED)
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 2014 on 925 degrees of freedom
# Multiple R-squared:  0.2624,	Adjusted R-squared:  0.2616 
# F-statistic:   329 on 1 and 925 DF,  p-value: < 2.2e-16

## RESIDUALS STANDARD ERRORS ARE VERY HIGH

# model fit: r-squared
summary(m1)$r.squared
# 0.2623768

# Assumptions of regression
# Normality of residuals
hist(m1$residuals, probability = TRUE)
lines(density(m1$residuals), col = "red")

## RESIDUALS MAY HAVE A KIND OF POSITIVE SKEWNESS

# QQ-plot
qqnorm(m1$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m1$residuals, col = "red")

# Test for Skewness and Kurtosis
# Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m1$residuals)
# Jarque-Bera Normality Test
# data:  m1$residuals
# JB = 104.66, p-value < 2.2e-16
# alternative hypothesis: greater

##P-VALUE IS LESS THAN 0.05, SO IT REJECTS THE NORMALITY OF RESIDUALS.

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m1$residuals)
# Anscombe-Glynn kurtosis test
# data:  m1$residuals
# kurt = 3.7320, z = 3.5512, p-value = 0.0003834
# alternative hypothesis: kurtosis is not equal to 3

##P-VALUE IS LESS THAN 0.05, SO IT REJECTS THE NORMALITY OF RESIDUALS.

#Diagnostic plots
plot(m1)


## Question 06: Second Reg., Single variable nonlinear regression---------------

plot(data1$KM, data1$Price, main = "Price vs KM", ylab = "Price", xlab = "KM")
abline(m1, col = "red", lwd = 2)

## DISTRIBUTION OF SCATTERED PLOT SHOWS A NONLINEAR BETWEEN PRICE AND KM

#Quadratic Regression
m2 <- lm(Price ~ KM + I(KM^2), data = train)

summary(m2)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6587.1 -1330.7  -318.2  1125.0  8505.4 
mean(m2$residuals)
# 2.69807e-14
## THERE IS A LARGE VALUE OF DIFFERENCE BETWEEN MEAN AND MEDIAN 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  1.354e+04  2.580e+02  52.478  < 2e-16 *** (P-VALUE IS ACCEPTED)
#  KM          -6.467e-02  5.932e-03 -10.902  < 2e-16 *** (P-VALUE IS ACCEPTED)
#  I(KM^2)      1.679e-07  3.030e-08   5.541 3.92e-08 *** (P-VALUE IS ACCEPTED)
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1983 on 924 degrees of freedom
# Multiple R-squared:  0.2861,	Adjusted R-squared:  0.2846 
# F-statistic: 185.1 on 2 and 924 DF,  p-value: < 2.2e-16

## RSE DECREASED AND MRS INCREASED. THIS MODEL (M2) IS BETTER THAN M1

# model fit: r-squared
summary(m2)$r.squared
# 0.2860992


# Assumptions of regression
# Normality of residuals
hist(m2$residuals, probability = TRUE)
lines(density(m2$residuals), col = "red")

# QQ-plot
qqnorm(m2$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m2$residuals, col = "red")

# Test for Skewness and Kurtosis
# Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m2$residuals)
# Jarque-Bera Normality Test
# data:  m2$residuals
# JB = 98.58, p-value < 2.2e-16
# alternative hypothesis: greater

## P-VALUE REJECTED NORMALITY OF RESIDUALS

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m2$residuals)
# Anscombe-Glynn kurtosis test
# data:  m2$residuals
# kurt = 3.9037, z = 4.1293, p-value = 3.639e-05
# alternative hypothesis: kurtosis is not equal to 3

## P-VALUE REJECTED NORMALITY OF RESIDUALS

#Diagnostic plots
plot(m2)

ggplot(train, aes(KM, Price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + 
  geom_smooth(method = "lm", formula = y ~ x, color = "red") + 
  ggtitle("Price vs KM")

## NONLINEAR MODEL IS CLOSER TO THE REAL DATA DISTRIBUTION THAN LINEAR MODEL
## BUT THERE ARE SOME OUTLIERS THAT CAUSES RESIDUALS NON-NORMAL

## Question 07: Removing Outliers------------------------
train2 <- train[ - which(rownames(train) == 79 | 
                           rownames(train) == 491 | 
                           rownames(train) == 1325),]
## THREE OBSERVATIONS ARE REMOVED
dim(train)
# 927  15
dim(train2)
# 924  15

m2_2 <- lm(Price ~ KM + I(KM^2), data = train2)

summary(m2_2)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5379.7 -1345.3  -311.9  1133.0  8562.5 
mean(m2_2$residuals)
# 4.687736e-14

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  1.374e+04  2.592e+02  53.002  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   KM          -6.866e-02  5.955e-03 -11.531  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   I(KM^2)      1.829e-07  3.039e-08   6.018 2.55e-09 *** (P-VALUE IS ACCEPTED)
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1957 on 921 degrees of freedom
# Multiple R-squared:  0.3039,	Adjusted R-squared:  0.3024 
# F-statistic: 201.1 on 2 and 921 DF,  p-value: < 2.2e-16

## RSE DECREASED AND MRS INCREASED. THIS MODEL (M2_2) IS BETTER THAN M2

# model fit: r-squared
summary(m2_2)$r.squared
# 0.3039197

# Assumptions of regression
# Normality of residuals
hist(m2_2$residuals, probability = TRUE)
lines(density(m2_2$residuals), col = "red")

# QQ-plot
qqnorm(m2_2$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m2_2$residuals, col = "red")

# Test for Skewness and Kurtosis
# Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m2_2$residuals)
# Jarque-Bera Normality Test
# data:  m2_2$residuals
# JB = 103.03, p-value < 2.2e-16
# alternative hypothesis: greater

## P-VALUE REJECTED NORMALITY OF RESIDUALS

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m2_2$residuals)
# Anscombe-Glynn kurtosis test
# data:  m2_2$residuals
# kurt = 3.8276, z = 3.8747, p-value = 0.0001067
# alternative hypothesis: kurtosis is not equal to 3

## P-VALUE REJECTED NORMALITY OF RESIDUALS

#Diagnostic plots
plot(m2_2)

#       (Intercept)          KM      I(KM^2)     MRS  RSE
#   m2     13537.12 -0.06466587 1.679225e-07  0.2861 1983
# m2_2     13738.80 -0.06866048 1.828770e-07  0.3039 1957

## HOWEVER THE RESIDUALS ARE NOT NORMAL, MRS AND RSE ARE GETTING BETTER

## Question 08: Quadratic Model---------------------

## M2 HAS A COLINEARITY PROBLEM BECAUSE THE RELATIONAL COEFFICIENTS ARE GREATER THAN 10
car :: vif(m2)
#       KM  I(KM^2) 
# 10.73222 10.73222 
## TO SOLVE THE COLINEARITY PROBLEM WE MAKE ANOTHER MODEL

train$KM_scaled <- scale(train$KM)
head(train)
m2_3 <- lm(Price ~ KM_scaled + I(KM_scaled^2), data = train)

summary(m2_3)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6587.1 -1330.7  -318.2  1125.0  8505.4 
mean(m2_3$residuals)
# 7.60365e-14

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)     9710.00      76.01 127.748  < 2e-16 *** (P-VALUE IS ACCEPTED)
#  KM_scaled      -1444.35      78.61 -18.375  < 2e-16 *** (P-VALUE IS ACCEPTED)
#  I(KM_scaled^2)   217.45      39.24   5.541 3.92e-08 *** (P-VALUE IS ACCEPTED)
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1983 on 924 degrees of freedom
# Multiple R-squared:  0.2861,	Adjusted R-squared:  0.2846 
# F-statistic: 185.1 on 2 and 924 DF,  p-value: < 2.2e-16

# model fit: r-squared
summary(m2_3)$r.squared
# 0.2860992

# Assumptions of regression
# Normality of residuals
hist(m2_3$residuals, probability = TRUE)
lines(density(m2_3$residuals), col = "red")

# QQ-plot
qqnorm(m2_3$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m2_3$residuals, col = "red")

# Test for Skewness and Kurtosis
# Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m2_3$residuals)
# Jarque-Bera Normality Test
# data:  m2_3$residuals
# JB = 98.58, p-value < 2.2e-16
# alternative hypothesis: greater

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m2_3$residuals)
# Anscombe-Glynn kurtosis test
# data:  m2_3$residuals
# kurt = 3.9037, z = 4.1293, p-value = 3.639e-05
# alternative hypothesis: kurtosis is not equal to 3

## UNFORTUNATELY RESIDUALS DO NOT HAVE NORMAL DISTRIBUTION

#       (Intercept)          KM        I(KM^2)     MRS  RSE
#   m2     13537.12 -0.06466587   1.679225e-07  0.2861 1983
# m2_2     13738.80 -0.06866048   1.828770e-07  0.3039 1957
#                     KM_scaled I(KM_scaled^2)
# m2_3.    9710.00     -1444.35         217.45  0.2861 1983

## MRS DECREASED AND RSE INCREASED (ALL THE TWO PARAMETERS GOT WORSE)

#Diagnostic plots
plot(m2_3)

car :: vif(m2_3)
# KM_scaled I(KM_scaled^2) 
#  1.455471       1.455471 
## COLINEARITY COEFS. ARE ACCEPTABLE

## Question 09: Total Reg------------------

m3 <- lm(Price ~ Age + FuelType + fHP + fMetColor + fAutomatic + fCC 
         + fDoors + Weight + KM_scaled + I(KM_scaled^2), data = train)

summary(m3)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5074.1  -652.5     0.0   637.9  3763.6 
mean(m3$residuals)
# -2.681207e-14
## WOW! MEDIAN AND MEAN ARE THE SAME

# Coefficients: (2 not defined because of singularities)
#               Estimate     Std. Error t value Pr(>|t|)    
#   (Intercept)     5179.815   2572.597   2.013   0.0444 *  
#   Age             -117.109      3.223 -36.331  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   FuelTypeDiesel   718.740   1548.430   0.464   0.6426    
#   FuelTypePetrol   650.780    322.340   2.019   0.0438 *  
#   fHP72           2247.539   1085.397   2.071   0.0387 *  
#   fHP86           2427.277   1857.199   1.307   0.1916    
#   fHP90           1968.596   1110.383   1.773   0.0766 .  
#   fHP97           3100.412   2139.814   1.449   0.1477    
#   fHP107          2783.043   1891.488   1.471   0.1415    
#   fHP110          2683.533   1855.091   1.447   0.1484    
#   fHP116          1715.102   1512.042   1.134   0.2570    
#   fMetColor1       -20.086     75.891  -0.265   0.7913    
#   fAutomatic1      443.981    183.018   2.426   0.0155 *  
#   fCC1332         -515.464   1071.696  -0.481   0.6306    
#   fCC1398        -1897.177   1311.611  -1.446   0.1484    
#   fCC1400         -749.753   1076.041  -0.697   0.4861    
#   fCC1587         -834.330    761.476  -1.096   0.2735    
#   fCC1598          884.613   1116.668   0.792   0.4285    
#   fCC1600               NA         NA      NA       NA    
#   fCC1800        -3119.725   1512.916  -2.062   0.0395 *  
#   fCC1900         1491.382   1100.227   1.356   0.1756    
#   fCC1975          725.641   1107.678   0.655   0.5126    
#   fCC2000               NA         NA      NA       NA    
#   fDoors3          364.621   1071.887   0.340   0.7338    
#   fDoors4          466.366   1077.597   0.433   0.6653    
#   fDoors5          584.086   1073.158   0.544   0.5864    
#   Weight             7.594      1.275   5.954 3.74e-09 *** (P-VALUE IS ACCEPTED)
#   KM_scaled       -419.868     52.944  -7.930 6.45e-15 *** (P-VALUE IS ACCEPTED)
#   I(KM_scaled^2)   -53.019     23.206  -2.285   0.0226 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1068 on 900 degrees of freedom
# Multiple R-squared:  0.7984,	Adjusted R-squared:  0.7925 
# F-statistic: 137.1 on 26 and 900 DF,  p-value: < 2.2e-16

## THE ACCEPTABLE VARIABLES: 
# Age, FuelTypePetrol, fHP72, fAutomatic1, fCC1800, Weight, KM_scaled, I(KM_scaled^2) 
# HERE WE HAVE JUST DROPPED fMetColor and fDoors.

m3 <- lm(Price ~ Age + FuelType + fHP + fAutomatic + fCC 
         + Weight + KM_scaled + I(KM_scaled^2), data = train)

summary(m3)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5211.4  -659.0     1.7   663.7  3856.8 
# 
# Coefficients: (2 not defined because of singularities)
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)     4516.860   2286.549   1.975   0.0485 *  
#   Age             -117.235      3.219 -36.424  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   FuelTypeDiesel   699.925   1549.597   0.452   0.6516    
#   FuelTypePetrol   695.486    321.256   2.165   0.0307 *  
#   fHP72           2136.478   1085.601   1.968   0.0494 *  
#   fHP86           2329.941   1860.292   1.252   0.2107    
#   fHP90           1747.355   1108.054   1.577   0.1152    
#   fHP97           3125.867   2142.721   1.459   0.1450    
#   fHP107          2655.924   1894.426   1.402   0.1613    
#   fHP110          2592.312   1858.163   1.395   0.1633    
#   fHP116          1702.493   1514.937   1.124   0.2614    
#   fAutomatic1      423.752    182.271   2.325   0.0203 *  
#   fCC1332         -580.203   1072.998  -0.541   0.5888    
#   fCC1398        -2088.230   1311.693  -1.592   0.1117    
#   fCC1400         -891.768   1075.707  -0.829   0.4073    
#   fCC1587         -843.155    762.525  -1.106   0.2691    
#   fCC1598          802.823   1116.741   0.719   0.4724    
#   fCC1600               NA         NA      NA       NA    
#   fCC1800        -3088.684   1515.771  -2.038   0.0419 *  
#   fCC1900         1392.239   1100.401   1.265   0.2061    
#   fCC1975          910.314   1107.336   0.822   0.4113    
#   fCC2000               NA         NA      NA       NA    
#   Weight             8.710      1.163   7.490 1.64e-13 *** (P-VALUE IS ACCEPTED)
#   KM_scaled       -419.040     52.989  -7.908 7.60e-15 *** (P-VALUE IS ACCEPTED)
#   I(KM_scaled^2)   -52.528     23.191  -2.265   0.0237 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1070 on 904 degrees of freedom
# Multiple R-squared:  0.7967,	Adjusted R-squared:  0.7917 
# F-statistic:   161 on 22 and 904 DF,  p-value: < 2.2e-16

## RSE AND MRS ARE GETTING BETTER BET IT IS BETTER TO REMOVE WHAT VARIABLE 
## DOES NOT WORK

## Question 10: Total Reg + IfPetrol------------------

train$IfPetrol <- ifelse(train$FuelType == "Petrol", TRUE, FALSE)
head(train)

m4 <- lm(Price ~ Age + Weight + KM_scaled + I(KM_scaled^2) + IfPetrol, data = train)

summary(m4)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5553.6  -640.2    -5.1   649.8  4016.9 
mean(m4$residuals)
# -3.950526e-14
##  MEAN HAS A SLIGHTLY DIFFERENCE WITH MEDIAN
 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    4452.679   1226.385   3.631 0.000298 *** (P-VALUE IS ACCEPTED)
#   Age            -112.236      2.858 -39.265  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Weight           10.764      1.033  10.421  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   KM_scaled      -419.758     53.330  -7.871 9.85e-15 *** (P-VALUE IS ACCEPTED)
#   I(KM_scaled^2)  -49.152     22.955  -2.141 0.032517 *  
#   IfPetrolTRUE    798.757    158.118   5.052 5.28e-07 *** (P-VALUE IS ACCEPTED)
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1092 on 921 degrees of freedom
# Multiple R-squared:  0.7842,	Adjusted R-squared:  0.783 
# F-statistic: 669.3 on 5 and 921 DF,  p-value: < 2.2e-16

# model fit: r-squared
summary(m4)$r.squared
# 0.7901083

car::vif(m4)
#      Age         Weight      KM_scaled I(KM_scaled^2)       IfPetrol 
# 1.461004       1.551597       2.208934       1.642091       1.984776 

# Assumptions of regression
# Normality of residuals
hist(m4$residuals, probability = TRUE)
lines(density(m4$residuals), col = "red")

# QQ-plot
qqnorm(m4$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m4$residuals, col = "red")

# Test for Skewness and Kurtosis
# Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m4$residuals)
# Jarque-Bera Normality Test
# data:  m4$residuals
# JB = 226.86, p-value < 2.2e-16
# alternative hypothesis: greater

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m4$residuals)
# Anscombe-Glynn kurtosis test
# data:  m4$residuals
# kurt = 5.2993, z = 7.3428, p-value = 2.092e-13
# alternative hypothesis: kurtosis is not equal to 3

## RESIDUALS ARE NOT NORMAL

#Diagnostic plots
plot(m4)
# IT HAS SOME OUTLIERS

## Question 11: Total ------------------
train3 <- train[ - which(rownames(train) == 112 | 
                           rownames(train) == 491 | 
                           rownames(train) == 850),]
dim(train)
# 927  17
dim(train3)
# 924  17
## THREE OBSERVATIONS REMOVED

m4_2 <- lm(Price ~ Age + Weight + KM_scaled + I(KM_scaled^2) + IfPetrol, data = train3)

summary(m4_2)
#Residuals:
#     Min      1Q  Median      3Q     Max 
# -4784.4  -651.0   -17.3   642.0  4498.2 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    -6060.419   1538.277  -3.940 8.78e-05 *** (P-VALUE IS ACCEPTED)
#   Age             -105.022      2.766 -37.968  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Weight            19.791      1.306  15.154  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   KM_scaled       -470.800     50.872  -9.255  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   I(KM_scaled^2)   -39.184     21.660  -1.809   0.0708 .  
#   IfPetrolTRUE    1352.097    161.590   8.367  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1026 on 918 degrees of freedom
# Multiple R-squared:  0.8095,	Adjusted R-squared:  0.8085 
# F-statistic: 780.1 on 5 and 918 DF,  p-value: < 2.2e-16

# model fit: r-squared
summary(m4_2)$r.squared
# 0.8094914

# Assumptions of regression
# Normality of residuals
hist(m4_2$residuals, probability = TRUE)
lines(density(m4_2$residuals), col = "red")

# QQ-plot
qqnorm(m4_2$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m4_2$residuals, col = "red")

# Test for Skewness and Kurtosis
# Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m4_2$residuals)
# Jarque-Bera Normality Test
# data:  m4_2$residuals
# JB = 117.89, p-value < 2.2e-16
# alternative hypothesis: greater

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m4_2$residuals)
# Anscombe-Glynn kurtosis test
# data:  m4_2$residuals
# kurt = 4.7197, z = 6.2378, p-value = 4.439e-10
# alternative hypothesis: kurtosis is not equal to 3

#Diagnostic plots
plot(m4_2)

train3 <- train3[ - which(rownames(train3) == 114 | 
                           rownames(train3) == 544 | 
                           rownames(train3) == 1325),]
dim(train)
# 927  17
dim(train3)
# 921  17

m4_2 <- lm(Price ~ Age + Weight + KM_scaled + I(KM_scaled^2) + IfPetrol, data = train3)

summary(m4_2)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -4775.5  -650.9   -36.1   641.0  3715.7 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    -8476.657   1535.091  -5.522 4.37e-08 *** (P-VALUE IS ACCEPTED)
#   Age             -102.319      2.727 -37.520  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Weight            21.789      1.303  16.728  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   KM_scaled       -493.571     50.102  -9.851  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   I(KM_scaled^2)   -28.584     21.302  -1.342     0.18    
#   IfPetrolTRUE    1491.619    159.063   9.378  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 999.1 on 915 degrees of freedom
# Multiple R-squared:  0.8185,	Adjusted R-squared:  0.8175 
# F-statistic: 825.3 on 5 and 915 DF,  p-value: < 2.2e-16

# model fit: r-squared
summary(m4_2)$r.squared
# 0.8184987

# Assumptions of regression
# Normality of residuals
hist(m4_2$residuals, probability = TRUE)
lines(density(m4_2$residuals), col = "red")

# QQ-plot
qqnorm(m4_2$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m4_2$residuals, col = "red")

# Test for Skewness and Kurtosis
# Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m4_2$residuals)
# Jarque-Bera Normality Test
# data:  m4_2$residuals
# JB = 71.561, p-value = 3.331e-16
# alternative hypothesis: greater

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m4_2$residuals)
# Anscombe-Glynn kurtosis test
# data:  m4_2$residuals
# kurt = 4.3393, z = 5.3502, p-value = 8.785e-08
# alternative hypothesis: kurtosis is not equal to 3

#Diagnostic plots
plot(m4_2)

train3 <- train3[ - which(rownames(train3) == 77 | 
                            rownames(train3) == 284 | 
                            rownames(train3) == 293),]
dim(train)
# 927  17
dim(train3)
# 918  17

m4_2 <- lm(Price ~ Age + Weight +KM_scaled +  I(KM_scaled^2) + IfPetrol, data = train3)

summary(m4_2)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3262.1  -648.4   -33.4   637.7  3598.8 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    -8145.929   1498.200  -5.437 6.95e-08 *** (P-VALUE IS ACCEPTED)
#   Age             -102.875      2.663 -38.634  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Weight            21.602      1.271  16.992  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   KM_scaled       -485.835     48.862  -9.943  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   I(KM_scaled^2)   -26.006     21.926  -1.186    0.236    
#   IfPetrolTRUE    1391.185    156.128   8.911  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 974.1 on 912 degrees of freedom
# Multiple R-squared:  0.8259,	Adjusted R-squared:  0.8249 
# F-statistic: 865.2 on 5 and 912 DF,  p-value: < 2.2e-16

# model fit: r-squared
summary(m4_2)$r.squared
# 0.8258945

# Assumptions of regression
# Normality of residuals
hist(m4_2$residuals, probability = TRUE)
lines(density(m4_2$residuals), col = "red")

# QQ-plot
qqnorm(m4_2$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m4_2$residuals, col = "red")

# Test for Skewness and Kurtosis
# Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m4_2$residuals)
# Jarque-Bera Normality Test
# data:  m4_2$residuals
# JB = 5.5808, p-value = 0.0614
# alternative hypothesis: greater

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m4_2$residuals)
# Anscombe-Glynn kurtosis test
# data:  m4_2$residuals
# kurt = 3.3739, z = 2.1048, p-value = 0.03531
# alternative hypothesis: kurtosis is not equal to 3

## WOW! P-VALUES ARE GETTING BETTER

#Diagnostic plots
plot(m4_2)

train3 <- train3[ - which(rownames(train3) == 269 | 
                            rownames(train3) == 944 | 
                            rownames(train3) == 948),]
dim(train)
# 927  17
dim(train3)
# 915  17

m4_2 <- lm(Price ~ Age + Weight + KM_scaled + IfPetrol, data = train3)

summary(m4_2)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3251.8  -648.3   -27.1   639.6  3400.6 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  -8124.298   1479.772  -5.490 5.21e-08 *** (P-VALUE IS ACCEPTED)
#   Age           -102.937      2.594 -39.684  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Weight          21.499      1.258  17.094  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   KM_scaled     -513.813     43.613 -11.781  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   IfPetrolTRUE  1460.450    151.897   9.615  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 962.7 on 910 degrees of freedom
# Multiple R-squared:  0.8298,	Adjusted R-squared:  0.8291 
# F-statistic:  1110 on 4 and 910 DF,  p-value: < 2.2e-16

# model fit: r-squared
summary(m4_2)$r.squared
# 0.829846

# Assumptions of regression
# Normality of residuals
hist(m4_2$residuals, probability = TRUE)
lines(density(m4_2$residuals), col = "red")

# QQ-plot
qqnorm(m4_2$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m4_2$residuals, col = "red")

# Test for Skewness and Kurtosis
# Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m4_2$residuals)
# Jarque-Bera Normality Test
# data:  m4_2$residuals
# JB = 1.9109, p-value = 0.3846
# alternative hypothesis: greater

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m4_2$residuals)
# Anscombe-Glynn kurtosis test
# data:  m4_2$residuals
# kurt = 3.2218, z = 1.3705, p-value = 0.1705
# alternative hypothesis: kurtosis is not equal to 3

#Diagnostic plots
plot(m4_2)

#Check for Multicollinearity
car :: vif(m4_2)
#      Age    Weight KM_scaled  IfPetrol 
# 1.533814  1.980649  1.759529  2.178224 

## m4 vs m4_2
#      (Intercept)        Age   Weight  KM_scaled IfPetrolTRUE    MRS   RSE
# m4    4452.67869 -112.23639 10.76447 -419.75811    798.75749 0.7842  1092
# m4_2 -8124.29783 -102.93687 21.49877 -513.81344   1460.44993  0.836 949.1

# PERCENT OF REMOVING DATA AS OUTLIERS: (927 - 915) * 100 / 927 ~ 1.3 % < 5 %

## Question 12: Prediction--------------------
#Test the model
test$KM_scaled <- scale(test$KM)
test$IfPetrol <- ifelse(test$FuelType == "Petrol", TRUE, FALSE)
dim(test)
dim(train3)
test$pred <- predict(m4_2, test)

plot(test$Price, test$pred, main = "Predicted vs Actual Price")
abline(a=0, b=1, col = "red", lwd = 2)
     
Etest <- abs(test$Price - test$pred)
medEtest <- median(Etest)
medEtest
# 661.5871
meanEtest <- mean(Etest)
meanEtest
# 846.6725
sdEtest <- sd(Etest)
sdEtest
# 734.8281
maxEtest <- max(Etest)
maxEtest
# 5626.332
minEtest <- min(Etest)
minEtest
# 1.125675
hist(Etest)
boxplot(Etest, main = "BoxPlot of Etest")

Epercent <- Etest/test$Price * 100
hist(Epercent)
sum(Epercent <= 15)/length(Epercent) * 100
# 83.9196
# YES 

## Question 13: Development-----------------

# It is a very functional and marketable platform. 
# More information is needed to make the pricing more realistic. 
# Like whether the car had an accident or not. 
# In this case, by adding information such as insurance history or the opinion of a 
#   specialized expert, the pricing process can be made more realistic. 
# On the other hand, in the real world, cars with different numbers of doors are not 
#   put together in the same price basket. 
# So maybe it is better to separate the data with different segmentation and measure 
#   each group separately. 


###End of Code ###-------------------------------
