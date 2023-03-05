# Final Project

getwd()

setwd("/users/tethys/documents/rmaktabkhooneh/lecture1")

dm <- read.table("/users/tethys/documents/rmaktabkhooneh/lecture1/DirectMarketing.csv", header = T, sep = ",")

# 1 summary of the project data set:
summary(dm)

# 2 How much percent of customers are women?
sum(dm$Gender == "Female")/nrow(dm)*100

# 3 How much percent of married men have salaries more than 50K?
sum(dm$Gender == "Male" & dm$Married == "Married" & dm$Salary > 50000)/sum(dm$Gender == "Male" & dm$Married == "Married")*100

# 4
library("ggplot2")
agesum <- table(dm$Age)
agesum
class(agesum)

agesum <- as.data.frame(agesum)
agesum

colnames(agesum) <- c("age", "count")
agesum <- agesum[order(agesum$count),]
agesum

ggplot(agesum, aes(x= reorder(age,count), y= count)) +
  geom_bar(stat = "identity") + 
  labs(
    title = "Age Distribution",
    x = "Age Category",
    y = "Frequency") + 
  ylim(0, 600)

# 5
ggplot(dm, aes(Age, fill = Gender)) +
  geom_bar() + 
  labs(
    title = "Age Distribution",
    x = "Age Category",
    y = "Frequency") + 
  ylim(0, 600) +
  scale_fill_manual(values = c('#42855B', '#A62349'))

# 6
head(dm)
hist(dm$AmountSpent, ylim = c(0, 300))

x <- dm$AmountSpent
hist(x, freq = F, xlim = c(0, max(x)), ylim = c(0, 6e-4), xlab = "Amount Spent", main = "Histogram with Exponential Curve")

lambda <- 1/mean(x)
xfit <- seq(0, max(x), length = 10000)
yfit <- dexp(xfit, rate = lambda)
lines(xfit,yfit, col = "Red" ,lwd = 2)


# 7
plot(dm$Catalogs)

boxplot(AmountSpent~Catalogs, data = dm, main = "Catalog - Purchase Relation", 
        xlab = "Number of Catalogues" , ylab = "Amount Spent")

# 8
plot(dm$Salary, dm$AmountSpent, xlab = "Salary" , ylab = "Amount Spent")
abline(lm(dm$AmountSpent~dm$Salary), col = "red", lw = 2)
title("Regression of Spending Amount over Salary")

# 9a
d <- data.frame(dm$Gender, dm$Salary, dm$AmountSpent)
head(d)
library(ggplot2)

ggplot(dm, aes(x= Gender, y= AmountSpent, col = Gender)) + 
  geom_boxplot() +
  labs(
    title = "Spending Amount by Gender")

median(dm$AmountSpent)

SA_M <- mean(dm$AmountSpent[dm$Gender=="Male"])
SA_M
SA_F <- mean(dm$AmountSpent[dm$Gender=="Female"])
SA_F

#9b
Sal_M <- mean(dm$Salary[dm$Gender=="Male"])
Sal_M
Sal_F <- mean(dm$Salary[dm$Gender=="Female"])
Sal_F

dm$incomelevel <- ifelse(dm$Salary > median(dm$Salary), "High Income","Low Income")

ggplot(dm, aes(x= Gender, y= AmountSpent, col = incomelevel)) + 
  geom_boxplot() +
  labs(
    title = "Spending Amount by Gender in Two Levels")


# 9c
ggplot(dm, aes(x= incomelevel, y= AmountSpent, group= Gender, colour = Gender)) +
  geom_point(shape = 1 , size =3) +
  stat_summary(fun = median, geom = "point", shape = 20, size = 5, color = "blue", fill = "blue")

ggplot(dm, aes(x= Gender, y=AmountSpent, group=incomelevel, colour = incomelevel)) +
  geom_line() +
  geom_point(shape = 1 , size =3)

median(dm$Salary)

IncomeLevel <- ifelse(dm$Salary > median(dm$Salary), "High Income", "Low Income")
new_dm <- cbind(dm,IncomeLevel)
head(new_dm)

MH <- mean(new_dm$AmountSpent[new_dm$Gender == "Male" & new_dm$IncomeLevel == "High Income"])
MH
ML <- mean(new_dm$AmountSpent[new_dm$Gender == "Male" & new_dm$IncomeLevel == "Low Income"])
ML
FH <- mean(new_dm$AmountSpent[new_dm$Gender == "Female" & new_dm$IncomeLevel == "High Income"])
FH
FL <- mean(new_dm$AmountSpent[new_dm$Gender == "Female" & new_dm$IncomeLevel == "Low Income"]) 
FL


Gender = c("Female","Female", "Male","Male")
IncomeLevel = c("High Income","Low Income","Low Income", "High Income")
AmountSpent = c(FH, FL, ML, MH)
df <- data.frame(Gender, IncomeLevel, AmountSpent)
head(df)

ggplot(df,aes(x = Gender , y = AmountSpent, group = IncomeLevel, col = IncomeLevel)) + 
  geom_line(size = 1) + 
  geom_point(shape = 1, size = 3) +
  ylim(500,1900) +
  labs(
    title = "Spending Amount by Gender in Two Levels of Salary")

#9c - Alternative
ggplot(dm, aes(x= incomelevel, y= AmountSpent, col = Gender)) + 
  geom_boxplot() +
  labs(
    title = "Spending Amount by Gender in Two Levels of Salary")
