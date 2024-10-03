#ANOVA is used to compare means of more than 2 independent variables(must be in categories)
#and the dependent variables MUST be numerical and continuous data i.e(numbers that change upwards)
#The independent variable must have at least three levels
#If the p-value result is >=0.05 then there is no significant difference in the means
#if p-value<=0.05 then there is a significant difference in means and the difference is not by chance

library(ggpubr)
library(tidyverse)
library(dplyr)

#Import data set
AnovaTest.data <- read.csv("C:\\Users\\Seroney\\Downloads\\Diet_R.csv")
view(AnovaTest.data)
str(AnovaTest.data)

#Test for normality and homogenity
#Assumption1: Shapiro-Wilkins test for diet1
with(AnovaTest.data, shapiro.test(weight6weeks[Diet=="1"]))

#Assumption2: Shapiro-Wilkins test for diet2
with(AnovaTest.data, shapiro.test(weight6weeks[Diet=="2"]))

#Assumption3: Shapiro-Wilkins test for diet3
with(AnovaTest.data, shapiro.test(weight6weeks[Diet=="3"]))

#Assumption4: Test for homogenity of the data using the bartlette.test
Hm_data <- bartlett.test(AnovaTest.data$weight6weeks ~ AnovaTest.data$Diet)
Hm_data

#Assumption4: the independent variable must be a factor
AnovaTest.data$Diet <- factor(AnovaTest.data$Diet)
str(AnovaTest.data$Diet)

#Method 1 of calculating the ANOVA between weight and diet:
One_Way_Test <- aov(AnovaTest.data$weight6weeks ~ AnovaTest.data$Diet)
One_Way_Test

#Method 2 of ANOVA calculation:
One_way_model <- lm(AnovaTest.data$weight6weeks ~ AnovaTest.data$Diet)
One_way_model

#Which of these groups have differences in means?
TukeyHSD(One_Way_Test)
TukeyHSD(aov(One_way_model))

#Plot the means in a boxplot
ggplot(AnovaTest.data, aes(x=Diet, y=weight6weeks, fill=Diet)) + 
  geom_boxplot()+ 
  geom_jitter(shape=15, color="steelblue", position=position_jitter(0.21))+
  theme_classic()

