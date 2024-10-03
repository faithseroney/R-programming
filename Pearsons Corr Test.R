#Tests the strength of the relationship between two variables
#When p<0.05 that's a strong correlation
#When p>0.05 that's a weak correlation
#In correlation, a change of magnitude in one variable results to a change in the other
#Pearson's correlation measures the relationship by drawing a line of best fit
#Assumptions: Independence, normality, homoscedasticity(equal variance), linearity.

#Install libraries
install.packages("devtools")

library(dplyr)
library(ggpubr)
library(devtools)

#Load and inspect the data
Correlation.data <- read.csv("C:\\Users\\Seroney\\Downloads\\trees.csv")
str(Correlation.data)

#Testing the correlation between girth and height
#Assumption: normality
shapiro.test(Correlation.data$Girth..in.)
shapiro.test(Correlation.data$Height..ft.)

#Correlation test when data is continuous
Pearson_Corr.Test <- cor.test(Correlation.data$Girth..in.,
                                Correlation.data$Height..ft.,method="pearson")
Pearson_Corr.Test

#Test for positive correlation
Pearson_Corr.Test1 <- cor.test(Correlation.data$Girth..in.,
                              Correlation.data$Height..ft.,method="pearson",
                              alternative = "greater")
Pearson_Corr.Test1

#Test for negative correlation
Pearson_Corr.Test2 <- cor.test(Correlation.data$Girth..in.,
                              Correlation.data$Height..ft.,method="pearson",
                              alternative = "less")
Pearson_Corr.Test2

#Another way to test for correlation is by plotting a scatter plot
ggscatter(Correlation.data, x = "Girth..in.", y = "Height..ft.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Girth (inches)", ylab = "Height (ft)", 
          main = "Correlation between Tree Girth and Height") 
          )


