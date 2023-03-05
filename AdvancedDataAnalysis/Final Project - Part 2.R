### Final Progect part 2 ------------------------
## Question 1: -----------------------------
setwd("/Users/tethys/Desktop/Advanced_Data_Analysis_R/Codes")
##Required libraries
#No library required
##Read data from file
data2 <- read.csv("dataset2.csv", header = TRUE)
names(data2)
# "Age"               "Experience"        "Income"            "Family"            "CCAvg"            
# "Education"         "Mortgage"          "SecuritiesAccount" "CDAccount"         "Online"           
# "CreditCard"        "PersonalLoan" 
dim(data2)
# 5000   12
head(data2)

## Question 2: --------------------------------
summary(data2)
#      Age          Experience       Income           Family          CCAvg          Education    
# Min.   :23.00   Min.   :-3.0   Min.   :  8.00   Min.   :1.000   Min.   : 0.000   Min.   :1.000  
# 1st Qu.:35.00   1st Qu.:10.0   1st Qu.: 39.00   1st Qu.:1.000   1st Qu.: 0.700   1st Qu.:1.000  
# Median :45.00   Median :20.0   Median : 64.00   Median :2.000   Median : 1.500   Median :2.000  
# Mean   :45.34   Mean   :20.1   Mean   : 73.77   Mean   :2.396   Mean   : 1.938   Mean   :1.881  
# 3rd Qu.:55.00   3rd Qu.:30.0   3rd Qu.: 98.00   3rd Qu.:3.000   3rd Qu.: 2.500   3rd Qu.:3.000  
# Max.   :67.00   Max.   :43.0   Max.   :224.00   Max.   :4.000   Max.   :10.000   Max.   :3.000  
#    Mortgage     SecuritiesAccount   CDAccount          Online         CreditCard     PersonalLoan  
# Min.   :  0.0   Min.   :0.0000    Min.   :0.0000   Min.   :0.0000   Min.   :0.000   Min.   :0.000  
# 1st Qu.:  0.0   1st Qu.:0.0000    1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.000  
# Median :  0.0   Median :0.0000    Median :0.0000   Median :1.0000   Median :0.000   Median :0.000  
# Mean   : 56.5   Mean   :0.1044    Mean   :0.0604   Mean   :0.5968   Mean   :0.294   Mean   :0.096  
# 3rd Qu.:101.0   3rd Qu.:0.0000    3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.000   3rd Qu.:0.000  
# Max.   :635.0   Max.   :1.0000    Max.   :1.0000   Max.   :1.0000   Max.   :1.000   Max.   :1.000  
str(data2)
# 'data.frame':	5000 obs. of  12 variables:
# $ Age              : int  25 45 39 35 35 37 53 50 35 34 ...
# $ Experience       : int  1 19 15 9 8 13 27 24 10 9 ...
# $ Income           : int  49 34 11 100 45 29 72 22 81 180 ...
# $ Family           : int  4 3 1 1 4 4 2 1 3 1 ...
# $ CCAvg            : num  1.6 1.5 1 2.7 1 0.4 1.5 0.3 0.6 8.9 ...
# $ Education        : int  1 1 1 2 2 2 2 3 2 3 ...
# $ Mortgage         : int  0 0 0 0 0 155 0 0 104 0 ...
# $ SecuritiesAccount: int  1 1 0 0 0 0 0 0 0 0 ...
# $ CDAccount        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Online           : int  0 0 0 0 0 1 1 0 1 0 ...
# $ CreditCard       : int  0 0 0 0 1 0 0 1 0 0 ...
# $ PersonalLoan     : int  0 0 0 0 0 0 0 0 0 1 ...

## THESE VARIABLES ARE CATEGORICAL, THEN WE MUST CHANGE THEM TO FACTORS:
## Family, Education, SecuritiesAccount, CDAccount, Online, CreditCard
## PersonalLoan SHOULD BE CATEGORICAL BECAUSE IT IS OUR DEPENDED VARIABLE
## HERE WE ALSO CHANGE THE LEVELS ("0", "1") TO ("NO", "YES")

data2$Family <- as.factor(data2$Family)

data2$Education <- as.factor(data2$Education)

data2$SecuritiesAccount <- as.factor(data2$SecuritiesAccount)
levels(data2$SecuritiesAccount) <- c("No", "Yes")

data2$CDAccount <- as.factor(data2$CDAccount)
levels(data2$CDAccount) <- c("No", "Yes")

data2$Online <- as.factor(data2$Online)
levels(data2$Online) <- c("No", "Yes")

data2$CreditCard <- as.factor(data2$CreditCard)
levels(data2$CreditCard) <- c("No", "Yes")

data2$PersonalLoan <- as.factor(data2$PersonalLoan)
levels(data2$PersonalLoan) <- c("No", "Yes")

str(data2)
# 'data.frame':	5000 obs. of  12 variables:
# $ Age              : int  25 45 39 35 35 37 53 50 35 34 ...
# $ Experience       : int  1 19 15 9 8 13 27 24 10 9 ...
# $ Income           : int  49 34 11 100 45 29 72 22 81 180 ...
# $ Family           : Factor w/ 4 levels "1","2","3","4": 4 3 1 1 4 4 2 1 3 1 ...
# $ CCAvg            : num  1.6 1.5 1 2.7 1 0.4 1.5 0.3 0.6 8.9 ...
# $ Education        : Factor w/ 3 levels "1","2","3": 1 1 1 2 2 2 2 3 2 3 ...
# $ Mortgage         : int  0 0 0 0 0 155 0 0 104 0 ...
# $ SecuritiesAccount: Factor w/ 2 levels "No","Yes": 2 2 1 1 1 1 1 1 1 1 ...
# $ CDAccount        : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
# $ Online           : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 2 2 1 2 1 ...
# $ CreditCard       : Factor w/ 2 levels "No","Yes": 1 1 1 1 2 1 1 2 1 1 ...
# $ PersonalLoan     : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 2 ...

## Question 3: --------------------

par(mfrow = c(2, 3))  # 2 rows and 3 columns
for (i in c(1, 2, 3, 5, 7)) {
  hist(data2[,i], xlab = "", main = paste("Histogram of", names(data2)[i]))
}
par(mfrow=c(1,1))

cor_table <- round(cor(data2[,c(1, 2, 3, 5, 7)]),2)
cor_table
#              Age Experience Income CCAvg Mortgage
# Age         1.00       0.99  -0.06 -0.05    -0.01
# Experience  0.99       1.00  -0.05 -0.05    -0.01
# Income     -0.06      -0.05   1.00  0.65     0.21
# CCAvg      -0.05      -0.05   0.65  1.00     0.11
# Mortgage   -0.01      -0.01   0.21  0.11     1.00

## THERE IS A VERY STRONG LINEAR RELATION BETWEEN Age AND Experience
## THERE IS A STRONG LINEAR RELATION BETWEEN Income AND CCAvg
plot(data2$Age, data2$Experience)
## PLOT SHOWS A PURE LINEAR RELATION BETWEEN Age AND Experience
## IT MAY HAS A BAD RESULT FOR OUR REGRESSION


table(data2$Family)
#    1    2    3    4 
# 1472 1296 1010 1222 

table(data2$Education)
#    1    2    3 
# 2096 1403 1501 

table(data2$SecuritiesAccount)
#   No  Yes 
# 4478  522 

table(data2$CDAccount)
#   No  Yes 
# 4698  302 

table(data2$Online)
#   No  Yes 
# 2016 2984 

table(data2$CreditCard)
#   No  Yes 
# 3530 1470 

table(data2$PersonalLoan)
#   No  Yes 
# 4520  480 

## Question 4: -------------------
#Devide dataset 
set.seed(1234)
train_cases <- sample(1 : nrow(data2), nrow(data2) * 0.8)
train <- data2[train_cases,]
test <- data2[- train_cases,]

summary(train)
#      Age          Experience        Income       Family       CCAvg        Education    Mortgage     
# Min.   :23.00   Min.   :-3.00   Min.   :  8.00   1:1191   Min.   : 0.000   1:1670    Min.   :  0.00  
# 1st Qu.:35.00   1st Qu.:10.00   1st Qu.: 38.00   2:1040   1st Qu.: 0.700   2:1131    1st Qu.:  0.00  
# Median :45.00   Median :20.00   Median : 64.00   3: 797   Median : 1.500   3:1199    Median :  0.00  
# Mean   :45.36   Mean   :20.13   Mean   : 73.71   4: 972   Mean   : 1.928             Mean   : 58.01  
# 3rd Qu.:55.00   3rd Qu.:30.00   3rd Qu.: 99.00            3rd Qu.: 2.500             3rd Qu.:104.00  
# Max.   :67.00   Max.   :43.00   Max.   :224.00            Max.   :10.000             Max.   :635.00  
# SecuritiesAccount CDAccount  Online     CreditCard PersonalLoan
# No :3573          No :3750   No :1633   No :2843   No :3618    
# Yes: 427          Yes: 250   Yes:2367   Yes:1157   Yes: 382
summary(test)
#      Age          Experience        Income       Family      CCAvg       Education    Mortgage     
# Min.   :23.00   Min.   :-3.00   Min.   :  8.00   1:281   Min.   :0.000   1:426     Min.   :  0.00  
# 1st Qu.:36.00   1st Qu.:10.00   1st Qu.: 40.00   2:256   1st Qu.:0.700   2:272     1st Qu.:  0.00  
# Median :45.00   Median :20.00   Median : 64.00   3:213   Median :1.600   3:302     Median :  0.00  
# Mean   :45.26   Mean   :19.99   Mean   : 74.02   4:250   Mean   :1.977             Mean   : 50.44  
# 3rd Qu.:55.00   3rd Qu.:29.00   3rd Qu.: 95.75           3rd Qu.:2.600             3rd Qu.: 83.00  
# Max.   :67.00   Max.   :43.00   Max.   :205.00           Max.   :9.300             Max.   :601.00  
# SecuritiesAccount CDAccount Online    CreditCard PersonalLoan
# No :905           No :948   No :383   No :687    No :902     
# Yes: 95           Yes: 52   Yes:617   Yes:313    Yes: 98 

## THERE IS NO NA DATA. HERE WE HAVE 4000 TRAIN DATA AND 1000 TEST DATA

## Question 5: -----------------
m1 <- glm(PersonalLoan ~ ., data = train, family = "binomial")
summary(m1)
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -3.0018  -0.1812  -0.0647  -0.0182   4.1586  
# 
# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)          -1.188e+01  2.124e+00  -5.593 2.23e-08 *** (P-VALUE IS ACCEPTED)
#   Age                  -4.151e-02  7.882e-02  -0.527 0.598426    
#   Experience            5.429e-02  7.831e-02   0.693 0.488180    
#   Income                6.264e-02  3.519e-03  17.803  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Family2              -2.256e-01  2.658e-01  -0.849 0.396075    
#   Family3               2.147e+00  2.891e-01   7.427 1.11e-13 *** (P-VALUE IS ACCEPTED)
#   Family4               1.839e+00  2.717e-01   6.769 1.30e-11 *** (P-VALUE IS ACCEPTED)
#   CCAvg                 1.929e-01  5.203e-02   3.707 0.000210 *** (P-VALUE IS ACCEPTED)
#   Education2            3.645e+00  3.054e-01  11.936  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Education3            3.947e+00  3.026e-01  13.042  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Mortgage              1.075e-03  6.816e-04   1.577 0.114855    
#   SecuritiesAccountYes -9.831e-01  3.381e-01  -2.908 0.003642 **  (P-VALUE MAY BE ACCEPTED)
#   CDAccountYes          3.757e+00  3.880e-01   9.683  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   OnlineYes            -6.867e-01  1.890e-01  -3.633 0.000281 *** (P-VALUE IS ACCEPTED)
#   CreditCardYes        -1.102e+00  2.464e-01  -4.471 7.79e-06 *** (P-VALUE IS ACCEPTED)
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2520.65  on 3999  degrees of freedom
# Residual deviance:  901.31  on 3985  degrees of freedom
# AIC: 931.31
# 
# Number of Fisher Scoring iterations: 8

## NUMBER OF PREDICTORS = 11
sum(train$PersonalLoan == "Yes") / nrow(train)
## PERCENT OF PR(Y=1) = 0.0955
# 10 * 11 / 0.0955
## THE NUMBER OF DATA WE REQUIER FOR LOGISTIC REGRESSION IS AT LEAST: 1152


car::vif(m1)
#                         GVIF Df GVIF^(1/(2*Df))
# Age               105.158681  1       10.254691
# Experience        104.869065  1       10.240560
# Income              2.905296  1        1.704493
# Family              1.629303  3        1.084760
# CCAvg               1.605662  1        1.267147
# Education           2.286115  2        1.229630
# Mortgage            1.040294  1        1.019948
# SecuritiesAccount   1.326424  1        1.151705
# CDAccount           1.998237  1        1.413590
# Online              1.136570  1        1.066100
# CreditCard          1.402256  1        1.184169

## THERE IS A MULTICOLINEARITY FOR Age AND Experience
## THIS TIME WE REMOVE ONE OF THE VARIABLES BETWEEN Age and Experience
## AND CHECK AGAIN:

m1 <- glm(PersonalLoan ~ Age + Income + Family + CCAvg + Education + Mortgage +
            SecuritiesAccount + CDAccount + Online + CreditCard, data = train, family = "binomial")
summary(m1)
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.9936  -0.1819  -0.0644  -0.0182   4.1621  
# 
# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)          -1.327e+01  7.381e-01 -17.973  < 2e-16 ***
#   Age                   1.286e-02  7.803e-03   1.648 0.099386 .  
#   Income                6.284e-02  3.508e-03  17.914  < 2e-16 ***
#   Family2              -2.176e-01  2.655e-01  -0.820 0.412416    
#   Family3               2.150e+00  2.892e-01   7.435 1.05e-13 ***
#   Family4               1.841e+00  2.719e-01   6.770 1.29e-11 ***
#   CCAvg                 1.924e-01  5.195e-02   3.704 0.000212 ***
#   Education2            3.636e+00  3.052e-01  11.913  < 2e-16 ***
#   Education3            3.922e+00  3.005e-01  13.051  < 2e-16 ***
#   Mortgage              1.058e-03  6.807e-04   1.554 0.120269    
#   SecuritiesAccountYes -9.738e-01  3.377e-01  -2.884 0.003932 ** 
#   CDAccountYes          3.762e+00  3.876e-01   9.707  < 2e-16 ***
#   OnlineYes            -6.858e-01  1.890e-01  -3.630 0.000284 ***
#   CreditCardYes        -1.100e+00  2.463e-01  -4.466 7.96e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2520.7  on 3999  degrees of freedom
# Residual deviance:  901.8  on 3986  degrees of freedom
# AIC: 929.8
# 
# Number of Fisher Scoring iterations: 8

## HERE WE SEE THE EFFECT OF AgeIS GETTIN BETTER (P-Value).
## BEFORE REMOVING Experience I CHECKED THE EFFECT OF SCALING
## I HAVE SEEN THAT IT CANNOT HELP
## WALD TEST APPROVED FOR P-VALUES UNDER 0.05 FOR ALL OF VARIABLES EXCEPT:
## Age, Family = 2, Mortgage
car::vif(m1)
#                       GVIF Df GVIF^(1/(2*Df))
# Age               1.031769  1        1.015761
# Income            2.895979  1        1.701758
# Family            1.628793  3        1.084703
# CCAvg             1.594153  1        1.262598
# Education         2.247762  2        1.224440
# Mortgage          1.039219  1        1.019421
# SecuritiesAccount 1.324080  1        1.150687
# CDAccount         2.001878  1        1.414877
# Online            1.136703  1        1.066163
# CreditCard        1.404432  1        1.185087

## SO THERE IS NO MULTILINEARITY PROBLEM

modelChi1 <- m1$null.deviance - m1$deviance
Chidf1 <- m1$df.null - m1$df.residual
Chisq_prob1 <- 1 - pchisq(modelChi1, Chidf1)
Chisq_prob1
# 0

## CHI-SQUARE TEST IS 0, SO THE PROCESS IS TRUSTABLE

## Question 6: ------------------
## HERE WE REMOVE UNRELIABLE VARIABLES

m2_1 <- glm(PersonalLoan ~ Income + Family + CCAvg + Education + SecuritiesAccount + 
            CDAccount + Online + CreditCard, data = train, family = "binomial")
summary(m2_1)
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -3.0204  -0.1848  -0.0653  -0.0186   4.1511  
# 
# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)          -12.580021   0.614464 -20.473  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Income                 0.063054   0.003497  18.033  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Family2               -0.207757   0.264204  -0.786 0.431662    
#   Family3                2.163522   0.289659   7.469 8.07e-14 *** (P-VALUE IS ACCEPTED)
#   Family4                1.852152   0.272213   6.804 1.02e-11 *** (P-VALUE IS ACCEPTED)
#   CCAvg                  0.177943   0.051563   3.451 0.000559 *** (P-VALUE IS ACCEPTED)
#   Education2             3.597877   0.302751  11.884  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Education3             3.873011   0.297147  13.034  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   SecuritiesAccountYes  -0.964371   0.337583  -2.857 0.004281 **  (P-VALUE MAY BE ACCEPTED)
#   CDAccountYes           3.790145   0.386912   9.796  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   OnlineYes             -0.667982   0.188075  -3.552 0.000383 *** (P-VALUE IS ACCEPTED)
#   CreditCardYes         -1.094532   0.245399  -4.460 8.19e-06 *** (P-VALUE IS ACCEPTED)
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2520.65  on 3999  degrees of freedom
# Residual deviance:  906.88  on 3988  degrees of freedom
# AIC: 930.88
# 
# Number of Fisher Scoring iterations: 8

modelChi21 <- m2_1$null.deviance - m2_1$deviance
Chidf21 <- m2_1$df.null - m2_1$df.residual
Chisq_prob21 <- 1 - pchisq(modelChi21, Chidf21)
Chisq_prob21
# 0

## NOW WE REMOVE family BECAUSE OF THE EFFECTS OF VALUE 1 AND VALUE 2 

m2_2 <- glm(PersonalLoan ~ Income + CCAvg + Education + SecuritiesAccount + 
              CDAccount + Online + CreditCard, data = train, family = "binomial")
summary(m2_2)
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.9812  -0.2209  -0.0906  -0.0274   3.8503  
# 
# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)          -10.729645   0.489557 -21.917  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Income                 0.054746   0.002967  18.452  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   CCAvg                  0.141687   0.046431   3.052 0.002276 **  (P-VALUE MAY BE ACCEPTED)
#   Education2             3.919970   0.282864  13.858  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   Education3             3.971382   0.275043  14.439  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   SecuritiesAccountYes  -0.991123   0.328828  -3.014 0.002577 **  (P-VALUE MAY BE ACCEPTED)
#   CDAccountYes           3.767178   0.363197  10.372  < 2e-16 *** (P-VALUE IS ACCEPTED)
#   OnlineYes             -0.665501   0.178834  -3.721 0.000198 *** (P-VALUE IS ACCEPTED)
#   CreditCardYes         -1.148169   0.235441  -4.877 1.08e-06 *** (P-VALUE IS ACCEPTED)
#   --- 
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2520.7  on 3999  degrees of freedom
# Residual deviance: 1015.6  on 3991  degrees of freedom
# AIC: 1033.6
# 
# Number of Fisher Scoring iterations: 8

modelChi22 <- m2_2$null.deviance - m2_2$deviance
Chidf22 <- m2_2$df.null - m2_2$df.residual
Chisq_prob22 <- 1 - pchisq(modelChi22, Chidf22)
Chisq_prob22
# 0

## HERE WE SEE IN m2_2 RESIDUAL DEVIANCE IS GREATER THAN m2_1
## AND THE EFFECT OF CCAvg IS GOT WORSE

## Question 7: -------------
test$probs21 <- predict(m2_1, test, type = "response") 
test$pred_lg21 <- ifelse(test$probs21 >= 0.5, 1, 0)
table(test$pred_lg21)
#   0   1 
# 921  79 
table(test$PersonalLoan)
#  No Yes 
# 902  98

## THE NUMBER OF WRONG PREDICTIONS = 19
## THE NUMBER OF RIGHT PREDICTIONS = 981
## PERCENT OF RIGHT PREDICTION: 981 / 1000 * 100 = 98.1 %

test$probs22 <- predict(m2_2, test, type = "response") 
test$pred_lg22 <- ifelse(test$probs22 >= 0.5, 1, 0)
table(test$pred_lg22)
#   0   1 
# 925  75 
table(test$PersonalLoan)
#  No Yes 
# 902  98

## THE NUMBER OF WRONG PREDICTIONS = 23
## THE NUMBER OF RIGHT PREDICTIONS = 977
## PERCENT OF RIGHT PREDICTION: 977 / 1000 * 100 = 97.7 %

table(test$PersonalLoan, test$pred_lg21)
#        0        1
# No  TN = 891 FP = 11
# Yes FN = 30  TP = 68

# Accuracy    = TP + TN / Total = (891 + 68) / (891 + 11 + 30 + 68) = 0.96
# Precision   = TP / (TP + FP)  = 68 / (68 + 11) = 0.86
# Sensitivity = TP / (TP + FN)  = 68 / (68 + 30) = 0.69
# Specificity = TN / (TN + FP)  = 891 / (891 + 11) = 0.99

table(test$PersonalLoan, test$pred_lg22)
#         0        1
# No   TN = 892 FP = 10
# Yes  FN = 33  TP = 65

# Accuracy    = TP + TN / Total = (892 + 65) / (892 + 10 + 33 + 65) = 0.96
# Precision   = TP / (TP + FP)  = 65 / (65 + 10) = 0.87
# Sensitivity = TP / (TP + FN)  = 65 / (65 + 33) = 0.66
# Specificity = TN / (TN + FP)  = 892 / (892 + 10) = 0.99


## Question 8: ---------------
# Naive Bayes
## 
set.seed(1234)
train_cases <- sample(1 : nrow(data2), nrow(data2) * 0.8)
trainNB <- data2[train_cases,]
testNB <- data2[- train_cases,]

dim(train)
# 4000   12
dim(test)
# 1000   12

library(e1071)
m3 <- naiveBayes(PersonalLoan ~., data = trainNB)
m3
# Naive Bayes Classifier for Discrete Predictors
# 
# Call:
#   naiveBayes.default(x = X, y = Y, laplace = laplace)
# 
# A-priori probabilities:
#   Y
#     No    Yes 
# 0.9045 0.0955 
# 
# Conditional probabilities:
#   Age
# Y       [,1]     [,2]
# No  45.36374 11.48700
# Yes 45.30890 11.72134
# 
# Experience
# Y       [,1]     [,2]
# No  20.13350 11.49238
# Yes 20.12304 11.74949
# 
# Income
# Y        [,1]     [,2]
# No   66.18436 40.76114
# Yes 145.00785 31.50599
# 
# Family
# Y           1         2         3         4
# No  0.3070757 0.2642344 0.1918187 0.2368712
# Yes 0.2094241 0.2198953 0.2696335 0.3010471
# 
# CCAvg
# Y       [,1]     [,2]
# No  1.724179 1.566724
# Yes 3.860471 2.080636
# 
# Education
# Y           1         2         3
# No  0.4402985 0.2739082 0.2857933
# Yes 0.2015707 0.3664921 0.4319372
# 
# Mortgage
# Y        [,1]      [,2]
# No   53.31813  92.68151
# Yes 102.47906 160.53828
# 
# SecuritiesAccount
# Y          No       Yes
# No  0.8946932 0.1053068
# Yes 0.8795812 0.1204188
# 
# CDAccount
# Y           No        Yes
# No  0.96213378 0.03786622
# Yes 0.70418848 0.29581152
# 
# Online
# Y          No       Yes
# No  0.4101714 0.5898286
# Yes 0.3900524 0.6099476
# 
# CreditCard
# Y          No       Yes
# No  0.7108900 0.2891100
# Yes 0.7094241 0.2905759

#Results
table(trainNB$PersonalLoan) / nrow(trainNB)
#     No    Yes 
# 0.9045 0.0955 

#Prediction
testNB$pred <- predict(m3, testNB)
head(testNB)
# Model Evaluation
table(testNB$PersonalLoan)
#  No Yes 
# 902  98 
table(testNB$pred)
#  No Yes 
# 886 114 

## THE NUMBER OF WRONG PREDICTIONS = 16
## THE NUMBER OF RIGHT PREDICTIONS = 984
## PERCENT OF RIGHT PREDICTION: 984 / 1000 * 100 = 98.4 %

# Confusion Matrix
table(test$PersonalLoan, test$pred)
#        No       Yes
# No  TN = 847  FP = 55
# Yes FN = 39   TP = 59

# Accuracy    = TP + TN / Total = (847 + 59) / (847 + 55 + 39 + 59) = 0.91
# Precision   = TP / (TP + FP)  = 59 / (59 + 55) = 0.52
# Sensitivity = TP / (TP + FN)  = 59 / (59 + 39) = 0.60
# Specificity = TN / (TN + FP)  = 847 / (847 + 55) = 0.94

## THE VALUES OF CONFUSION MATRIX GIVES BETTER RESULTS FOR M2 THAN M3
## IN M2 WE CAN WORK WITH DATA, WE CAN REDUCE IT, CHANGE IT AND ANY OTHER SELECTIONS
## STEP BY STEP BUT NAIVE BAYES LOOKS LIKE A BLACK BOX. SO IN WORKING WITH DATA AS A
## STUDENT I PREFER M2 MODEL, BUT FOR A PROFESSIONAL PROJECT I PREFER NAIVE BAYES,
## BECAUSE ITS RIGHT PREDICTION PERCENT IS BETTER THAN M2 MODEL.

###End of the Code-------------------------------