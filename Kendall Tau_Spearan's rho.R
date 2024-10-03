#Tests the strength of the relationship n between two categorical or ordinal variables.
#When p<0.05 that's a strong correlation
#When p>0.05 that's a weak correlation
#In correlation, a change of magnitude in one variable results to a change in the other
#Assumptions: Independence, normality, homoscedasticity(equal variance), linearity.
#If the data does not qualify in these assumptions, we use Kendall Tau/Spearman's rho tests
#Otherwise use pearson's correlation test

#Install the haven package to read .dta files
install.packages("haven")
install.packages("foreign")

#Load the packages
library(ggpubr)
library(devtools)
library(haven)
library(foreign)

#Load the dataset
Tau_rho.data <- read.dta("C:\\Users\\Seroney\\Downloads\\lifeexp.dta")
str(Tau_rho.data)

#Test for assumptions
Tau_rho.data %>%
  group_by(region) %>%
  summarise(`W Stat` = shapiro.test(lexp)$statistic,
            p.value= shapiro.test(lexp)$p.value)

#Convert the region variable to numeric data
Tau_rho.data$region <- as.numeric(Tau_rho.data$region)
str(Tau_rho.data)

#Method1: Kendall's Tau where data is ordinal (Two-tailed)
Tau.test<- cor.test(Tau_rho.data$region, Tau_rho.data$lexp, method="kendall")
Tau.test

#Method2: Kendall's Tau test for positive correlation
Tau.test2<- cor.test(Tau_rho.data$region, Tau_rho.data$lexp, method="kendall",
                    alternative="greater")
Tau.test2

#Method2: Kendall's Tau test for negative correlation
Tau.test3<- cor.test(Tau_rho.data$region, Tau_rho.data$lexp, method="kendall",
                     alternative="less")
Tau.test3

#Method2: Spearman's rho test when data is ordinal
Spearman.rho_test <- cor.test(Tau_rho.data$region, Tau_rho.data$lexp, 
                              method = "spearman", exact = FALSE)
Spearman.rho_test

#Spearman's test for positive correlation
Spearman.rho_test2 <- cor.test(Tau_rho.data$region, Tau_rho.data$lexp, 
                              method = "spearman", alternative = "greater",
                              exact = FALSE)
Spearman.rho_test2

#Spearman's test for negative correlation
Spearman.rho_test3 <- cor.test(Tau_rho.data$region, Tau_rho.data$lexp, 
                               method = "spearman", alternative = "less",
                               exact = FALSE)
Spearman.rho_test3

# Step 4 - Visualize Correlation between the two variables

# Method 1: Kendall's tau

ggscatter (Tau_rho.data, x = "region", y = "lexp",
           add = "reg.line", conf.int = TRUE,
           cor.coef = TRUE, cor.method = "kendall",
           xlab = "Region (demographic)", ylab = "Life expectancy (age) ",
           main = "Correlation between Region and Life expectancy ")

# Method 2: Spearman's rho
ggscatter (Tau_rho.data, x = "region", y = "lexp",
           add = "reg.line", conf.int = TRUE,
           cor.coef = TRUE, cor.method = "spearman",
           xlab = "Region (demographic) ", ylab = "Life expectancy (age) ",
           main = "Correlation between Region and Life expectancy ")
