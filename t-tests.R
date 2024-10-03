#t-tests are used to compare the means of two sets of independent data
#T = sample mean difference/sample standard deviation of the sample mean difference
#Independent t-test is performed on two samples of data that are independent

install.packages("rstatix") 

library(dplyr)
library(ggpubr)
library(rstatix)
library(car)
library(tidyverse)

T_test.data = read.csv("C:\\Users\\Seroney\\Downloads\\Cholesterol_R.csv")
view(T_test.data)

#Test for assumptions before analyzing the data

#Assump1: Shapiro Wilkins test for normality
with(T_test.data, shapiro.test(Before[Margarine =="A"]))

#Assump2: Shapiro Wilkins test for normality of sample B
with(T_test.data, shapiro.test(Before[Margarine=="B"]))

#Assumption3: f-test to test the homogenity of the data
homogenity.ftest <- var.test(Before~Margarine, data = T_test.data)
homogenity.ftest

# Independent (Unpaired) t-test where y is numeric and x is a binary factor (Two-tailed)
independent.tTest <- t.test(Before ~ Margarine, data = T_test.data, var.equal=TRUE)
independent.tTest

# Test whether the Ave. of Margarine group A is less than the Ave. of Margarine group B (One-tailed) 
independent.tTest2 <- t.test(Before~Margarine, data=T_test.data, var.equal=TRUE,
                             alternative="less")
independent.tTest2 

# Test whether the Ave. of Margarine group A is greater than the Ave. of Margarine group B (One-tailed) 
independent.tTest3 <- t.test(Before~Margarine, data=T_test.data, var.equal=TRUE,
                             alternative="greater")
independent.tTest3

#Another way to test the means is  to plot the differences with a boxplot
ggboxplot(T_test.data, x="Margarine", y="Before", color = "Margarine",
          palette = c("dark red", "navy blue"),
          ylab = "Before", xlab = "Margarine")



