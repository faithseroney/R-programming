#Used to test whether a group of data comes from the same population 
#By considering the effect of the independent variable on the dependent variable
#Performed when data does not satisfy the assumption of t-distribution(not normally distributed)
#Thus, the tests are  performed using the median as opposed to the mean
#Independent variable must be of two levels or groups
#Is the non-parametric equivalent of the independent sample t-test

#Install libraries
install.packages("FSA")
install.packages("gmodels")
install.packages("PMCMRplus")
install.packages("DescTools")

#Load the libraries
library(FSA)
library(gmodels)
library(PMCMRplus)
library(DescTools)
library(car)
library(ggpubr)
library(dplyr)

#Load the data
Mann_Whit.data<- read.csv("C:\\Users\\Seroney\\Downloads\\sample-csv-file-for-testing (2).csv")
attach(Mann_Whit.data)
str(Mann_Whit.data)

#We will test if there's a significant difference in units sold between 2013 & 2014
#Convert the numerical variable to a factor
Mann_Whit.data$Year <- as.factor(Mann_Whit.data$Year)
str(Mann_Whit.data)

#Conduct the test

#Test for normality of the data(year) to ensure that the data is not normal hence
# the need to use Mann-Whitney's test to test the relationship

Mann_Whit.data %>%
  group_by(Year) %>%
  summarise(w.test = shapiro.test(Units.Sold)$statistic,
            p.value = shapiro.test(Units.Sold)$p.value)

#Perform the test using Wilcox.test()
MannWhitneyTest <- wilcox.test(Mann_Whit.data$Units.Sold ~ Mann_Whit.data$Year,
                               conf.int = TRUE)
MannWhitneyTest





