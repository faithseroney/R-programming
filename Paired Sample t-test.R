# Used to test the means of two sets of data. But the data should be from same group 
# but taken during different time intervals.

install.packages("PairedData")

library(tidyverse)
library(car)
library(dplyr)
library(ggpubr)
library(rstatix)
library(PairedData)

T_test.data = read.csv("C:\\Users\\Seroney\\Downloads\\Cholesterol_R.csv")
view(T_test.data)

#Make assumptions about the data:
#Assumption1: to test if data is nominal: Shapiro-Wilkins test.
shapiro.test(T_test.data$After4weeks)
shapiro.test(T_test.data$After8weeks) #Both have W>0.5 and p>0.05 hence normally distributed

#Assumption2: f-test to determine the homogeneity of the variance
homogenity_ftest <- var.test(T_test.data$After4weeks, T_test.data$After8weeks)
homogenity_ftest
  #p=0.9376(>0.05) hence data is homogeneous i.e equal in variance

#2-tailed paired t-test where both variables must be numerical
Paired_tTest <- t.test(T_test.data$After4weeks, T_test.data$After8weeks, 
                       var.equal = TRUE, paired = TRUE)
Paired_tTest #Results in p=0.001491(<0.05) hence the two means are statistically diff

#One-tailed t-test to test whether the avg of after4weeks is less than the avg
#of after8weeks
Paired_tTest2 <-  t.test(T_test.data$After4weeks, T_test.data$After8weeks, 
                         var.equal = TRUE, paired = TRUE, alternative = "less")
Paired_tTest2 #Results in p=0.9993(>0.05) hence the mean of after4weeks is not less

#One-tailed t-test to test whether the avg of after4weeks is greater than the avg
#of after8weeks
Paired_tTest3 <-  t.test(T_test.data$After4weeks, T_test.data$After8weeks, 
                         var.equal = TRUE, paired = TRUE, alternative = "greater")
Paired_tTest3 #Results in p=0.0007457(<0.05) hence the mean of after4weeks is greater
 
#Plot the paired t-test
pairedSample <- paired(T_test.data$After4weeks, T_test.data$After8weeks)
plot(pairedSample, type = "profile", col = c("blue", "red"))

