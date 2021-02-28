# Clinton Corbin
# Colorado State University Global
# MIS480: Capstone – Business Analytics and Information Systems
# Steve Chung, Ph.D.
# March 7, 2021

# Load the data to be worked with:
setwd("C:/Users/clinton/OneDrive/Documents/CSU Global/MIS480 - Capstone/data")
mydata=read.csv("BitcoinHeistData.csv")

# Examine the structure and first few rows of the data:
str(mydata)

# set the environment
# set the R mirror to use:
chooseCRANmirror()

#grab dplyr, janitor, tidyr, PerformanceAnalytics packages and use as needed
utils:::menuInstallPkgs()
library(janitor)
library(dplyr)
library(tidyr)
library(PerformanceAnalytics)

# To get count and mean of each transaction and merge the two tables:
ransomware_count<-aggregate(mydata$label, list(mydata$label), FUN=length)
ransomware_mean<-aggregate(mydata$income, list(mydata$label), FUN=mean)
ransomware_summary<-merge(ransomware_count, ransomware_mean, by="Group.1")
ransomware_summary %>%
  rename(transaction_count = x.x) %>%
  rename(category=Group.1) %>%
  rename(transaction_mean = x.y)

# build a table of quantitative variables for analysis
my_data_quant <- mydata[, c(4,5,6,7,8,9)]

# check it for any correlation in the variables
cor(my_data_quant)

# display the correlation chart for the quantitative variables
chart.Correlation(my_data_quant, histogram=TRUE, pch=19)

# perform regression analysis...
# null hypothesis - length determines income
# perform the analysis using lm() and store in length_income_relation
length_income_relation<-lm(mydata$income~mydata$length)

# examine the results:
print(summary(length_income_relation))

# now graph the results:
png(file="length_income_linear_regression.png")
plot(mydata$income,mydata$length, col = "blue",
  main = "Length & Income Regression",
  abline(lm(mydata$length~mydata$income)),
  cex = 1.3, pch = 16, 
  xlab="Length",ylab="Income")
dev.off()

# regression analysis #2
# null hypothesis - looped determines income
# perform the analysis using lm() and store in looped_income_relation
looped_income_relation <- lm(mydata$income~mydata$looped)

#examine the results
print(summary(looped_income_relation))

# now graph the results:
png(file = "looped_income_linear_regression.png")
plot(mydata$income,mydata$looped, col = "blue",
  main = "Looped & Income Regression",
  abline(lm(mydata$looped~mydata$income)),
  cex = 1.3, pch = 16, 
  xlab="Looped",ylab="Income")
dev.off()
