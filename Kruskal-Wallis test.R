#Used to test whether a group of data comes from the same population 
#By considering the effect of the independent variable on the dependent variable
#Performed when data does not satisfy the assumption of t-distribution(not normally distributed)
#Thus, the tests are  performed using the median as opposed to the mean
#Independent variable must be of more than two levels or groups
#Is the non-parametric equivalent of the one-way anova test

#Load the libraries
library(FSA)
library(PMCMRplus)
library(ggpubr)
library(dplyr)
library(DescTools)

#Load the data:
Kruskal_Wallis.data <- read.csv("C:\\Users\\Seroney\\Downloads\\sample-csv-file-for-testing (2).csv")
str(Kruskal_Wallis.data)

#Convert the independent variable into a factor
Kruskal_Wallis.data$Country <- as.factor(Kruskal_Wallis.data$Country)
str(Kruskal_Wallis.data)

#Test for the normality of the data
Kruskal_Wallis.data %>% 
  group_by(Country) %>% 
  summarise(`W Stat` = shapiro.test(Units.Sold)$statistic, 
            p.value = shapiro.test(Units.Sold)$p.value) 

#Perform the Kruskal-Wallis test
KruskalWallisTest <- kruskal.test(Kruskal_Wallis.data$Units.Sold ~ 
                                    Kruskal_Wallis.data$Country )
KruskalWallisTest

#Since the p-value is <0.05, we need to perform a dunn test using bonferroni method
PostHOCTest_1 <- dunnTest(Units.Sold~Country, data = Kruskal_Wallis.data,
                          method = "bonferroni")
PostHOCTest_1

#Plot the data in a boxplot
ggplot(Kruskal_Wallis.data, aes(x = Country, y = Units.Sold, fill = 
                                    Country)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "grey") +
  stat_summary(fun = mean, geom="point", shape=10, size=3.5, color="black")+
  ggtitle("Boxplot of distribution (median) of Units.Sold by Country") +
  theme_bw() + theme(legend.position="none")

