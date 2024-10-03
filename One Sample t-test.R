#used to compare the means of a set of data against a standard mean value

library(ggpubr)
library(tidyverse)
library(rstatix)
library(dplyr)
library(car)

#We will analyse the mean of after8weeks 
#Check if the mean of After8weeks is equal to 5(two-tailed)
oneSample.test <- t.test(T_test.data$After8weeks, mu=5, alternative = "two.sided")
oneSample.test

#Check if the mean of After8weeks is less than 5(one-tailed)
oneSample.test2 <- t.test(T_test.data$After8weeks, mu=5, alternative = "less")
oneSample.test2

#Check if the mean of After8weeks is greater than 5(one-tailed)
oneSample.test3 <- t.test(T_test.data$After8weeks, mu=5, alternative = "greater")
oneSample.test3

#plot the data in a boxplot
ggboxplot(T_test.data$After8weeks, 
          ylab = "After8weeks (values)", xlab = FALSE,
          ggtheme = theme_minimal()) 
ggqqplot(T_test.data$After8weeks, ylab = "Distribution after 8 
weeks",
         ggtheme = theme_minimal())
