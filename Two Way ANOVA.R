# 2way ANOVA compares two sets of independent variables(with each set having more
#than 2 groups) against a dependent continuous variable
#As always, the means have no significant differences when P>=0.05

#Load the libraries
library(tidyverse)
library(ggpubr)
library(dplyr)

#Load the data
AnovaTest.data <- read.csv("C:\\Users\\Seroney\\Downloads\\Diet_R.csv")
str(AnovaTest.data)

#Data cleaning to remove NA:
AnovaTest.data <- na.omit(AnovaTest.data)
AnovaTest.data

#We are going to check how gender and diet affect the weights of chicks after 6wks
#Check the assumptions:
#Assumption1: Independent variables (gender and diet) must be a factor
AnovaTest.data$gender <- factor(AnovaTest.data$gender)
str(AnovaTest.data$gender)
AnovaTest.data$Diet <- factor(AnovaTest.data$Diet)
str(AnovaTest.data$Diet)

#Assumption2: Shapiro-Wilkins test for normality
anov_tst <- lm(weight6weeks~Diet * gender, data = AnovaTest.data)
anov_resd <- residuals(anov_tst)
shapiro.test(anov_resd)

#Assumption3: test for homogeneity in the data
hm_Test <- leveneTest(weight6weeks~Diet * gender, data = AnovaTest.data)
hm_Test

#We now conduct the ANOVA test with 2 methods:
#Method 1: Using the aov() function
TwoWayTest <- aov(weight6weeks~Diet * gender, data = AnovaTest.data)
summary(TwoWayTest)

#Method2: using the lm() function then aov()
TwoWayModel <- lm(weight6weeks~Diet * gender, data = AnovaTest.data)
aov(TwoWayModel)

#Post-hoc summary
TukeyHSD(TwoWayTest)

TukeyHSD(aov(TwoWayModel))

#plot the data on ggline
ggline(AnovaTest.data, x="Diet", y="weight6weeks", color="gender",
       add = c("mean_se", "dotplot"),
       palette = c("navy", "dark red"))







