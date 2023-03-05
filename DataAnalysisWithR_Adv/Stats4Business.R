# Final Project Stats4Business

data <- read.csv("./Atlas_Khodro.csv", header = T)
head(data)
tail(data)
summary(data)

# Answer 1
#Histogram
hist(data$Time_On_Line, probability = T, breaks = 15)
lines(density(data$Time_On_Line), col = "red")

#QQ-plot

qqnorm(data$Time_On_Line, main = "QQ Plot of data$Time_On_Line", pch = 20)
qqline(data$Time_On_Line, col = "red")

#Shapiro-Wilk Test for Normality 
#p-value < 0.05 reject normality assumption
#Good for sample size <= 25
shapiro.test(data$Time_On_Line)
# Answer:
# data:  data$Time_On_Line
# W = 0.98199, p-value = 0.6386

#Test for Skewness and Kurtosis
#Good for sample size > 25
install.packages("moments")
library(moments)

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption

jarque.test(data$Time_On_Line)
# Answer:
# data:  data$Time_On_Line
# JB = 0.095754, p-value = 0.9533
# alternative hypothesis: greater

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption

anscombe.test(data$Time_On_Line)
# Answer:
#data:  data$Time_On_Line
#kurt = 2.83100, z = 0.12428, p-value = 0.9011
#alternative hypothesis: kurtosis is not equal to 3

# End of Answer 1

# Answer 2
mean(data$Time_On_Line)
# 14.7788
var(data$Time_On_Line)
# 7.124292
# End of Answer 2

# Answer 3
NROW(data$Time_On_Line[data$Time_On_Line > 15])/NROW(data$Time_On_Line)*100
# % 46
# End of Answer 3

# Answer 4
sum(data$Number_Accepting_Insurance[!is.na(data$Number_Accepting_Insurance)])/(30*250)*100
# or
sum(data$Number_Accepting_Insurance, na.rm = T)/(30*250)*100
# % 61.12
# End of Answer 4

# Answer 5
p = 0.6112
n = 2
# Probability of exactly 2 sales (P(x=2))
dbinom(2, n, p)
# % 37.3565 
# End of Answer 5

# Answer 6
p = 0.6112
n = 10
# Probability of at least 5 sales (P(x>=5))
pbinom(4, n, p, lower.tail = FALSE)
# % 85.1875
# End of Answer 6

# Answer 7
# In this experiment we used dbinom and pbinom 
# End of Answer 7

# Answer 8, 9, 10

table(data$Complaints_per_20_Customers)
#  0  1  2  3 
# 11  7  4  3
poisson.test(x = sum(data$Complaints_per_20_Customers, na.rm = T), T = 25)
# Answer
#data:  sum(data$Complaints_per_20_Customers, na.rm = T) time base: 25
#number of events = 24, time base = 25, p-value = 1
#alternative hypothesis: true event rate is not equal to 1
#95 percent confidence interval:
#  0.6150901 1.4284039
#sample estimates:
#  event rate 
# 0.96 

dpois(4, mean(data$Complaints_per_20_Customers, na.rm = T))
# % 1.3551

ppois(3, mean(data$Complaints_per_20_Customers, na.rm = T), lower.tail = F)
# % 1.6633

# End of Answer 8, 9, 10

# Answer 11, 12, 13

mean(data$Wait_Time_for_Bus)
# -0.7102
NROW(data$Wait_Time_for_Bus[data$Wait_Time_for_Bus > +5])/NROW(data$Wait_Time_for_Bus)*100
# % 6

x = data$Wait_Time_for_Bus
y = log(data$Wait_Time_for_Bus + 10)
hist(x)
hist(y)
#Histogram
hist(x, probability = T, breaks = 15)
lines(density(x), col = "red")
hist(y, probability = T, breaks = 15)
lines(density(y), col = "red")

#QQ-plot

qqnorm(x, main = "QQ Plot of data$Time_On_Line", pch = 20)
qqline(x, col = "red")

qqnorm(y, main = "QQ Plot of data$Time_On_Line", pch = 20)
qqline(y, col = "red")

#Shapiro-Wilk Test for Normality 
#p-value < 0.05 reject normality assumption
#Good for sample size <= 25
shapiro.test(x)
# Shapiro-Wilk normality test
# data:  x
# W = 0.94992, p-value = 0.03387
shapiro.test(y)
#Shapiro-Wilk normality test
# data:  y
# W = 0.98814, p-value = 0.8935

#Test for Skewness and Kurtosis
#Good for sample size > 25
install.packages("moments")
library(moments)

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption

jarque.test(x)
# Jarque-Bera Normality Test
# data:  x
# JB = 10.211, p-value = 0.006064
# alternative hypothesis: greater
jarque.test(y)
# Jarque-Bera Normality Test
# data:  y
# JB = 0.52775, p-value = 0.7681
# alternative hypothesis: greater

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption

anscombe.test(x)
# Anscombe-Glynn kurtosis test
# data:  x
# kurt = 4.2951, z = 1.9030, p-value = 0.05705
# alternative hypothesis: kurtosis is not equal to 3
anscombe.test(y)
# Anscombe-Glynn kurtosis test
# data:  y
# kurt = 2.93832, z = 0.31746, p-value = 0.7509
# alternative hypothesis: kurtosis is not equal to 3

# End of Answer 11, 12, 13

# Answer 14, 15, 16, 17

mean(data$Gas_To_Fill)
# 18.52

NROW(data$Gas_To_Fill[data$Gas_To_Fill >= 40])/NROW(data$Gas_To_Fill)*100
# % 10

# Geometric distribution. 
p <- 0.1
x <- 3
# Probability that the first "failure" will be the 3rd experiment.
dgeom(x, p)
# % 7.29

hist(data$Gas_To_Fill, probability = T, breaks = 15)
lines(density(data$Gas_To_Fill), col = "red")

data$Gas_To_Fill
library(exptest)
shapiro.exp.test(data$Gas_To_Fill)
#Shapiro-Wilk test for exponentiality
#data:  data$Gas_To_Fill
#W = 0.018005, p-value = 0.2535

# End of Answer 13, 14, 15, 16

# Answer 18
x3 = data$Time_On_Line
y3 = data$Time_On_Line_Improved

mean(x3)
# 14.7788
var(x3)
# 7.1242
mean(y3)
# 11.3702
var(y3)
# 2.2396

# var.test for these two datasets
var.test(x3,y3)
# data:  x3 and y3
# F = 3.181, num df = 49, denom df = 49, p-value = 8.656e-05
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  1.805149 5.605546
# sample estimates:
#  ratio of variances 
# 3.181013 

# t-test for paired-group

# H0: mu0 = mu1
# H1: mu0 > mu1
# p-value < 0.05 rejects null hypothesis
t.test(x3, y3, alternative = "greater", conf.level = 0.95)
# Welch Two Sample t-test
# data:  x3 and y3
# t = 7.8764, df = 77.037, p-value = 8.77e-12
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#  2.688074      Inf
# sample estimates:
#   mean of x mean of y 
# 14.77880  11.37024 

# End of Answer 18

# Answer 19

# Number of sold insurance: 61.12 * 7500 = 4584 within 7500 cases
# Number of sold insurance in the new sample: 67 * 250 = 167.5 within 250 cases

# Proportion test, two sample
# H0: p0 = p1
# H1: p0 # p1
# p-value < 0.05 rejects null hypothesis
prop.test(x = c(4584,167.5), n = c(7500,250), alternative = "two.sided", conf.level = 0.95)
#2-sample test for equality of proportions with continuity correction
# data:  c(4584, 167.5) out of c(7500, 250)
# X-squared = 3.2828, df = 1, p-value = 0.07001
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.12018868  0.00258868
# sample estimates:
#   prop 1 prop 2 
# 0.6112 0.6700

# End of Answer 19

