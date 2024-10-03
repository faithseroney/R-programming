#Tests how observed data compares with expected data
# The compared data must be categorical, raw data, and must be mutually exclusive data.

install.packages("vcd")
install.packages("grid")

library(vcd)
library(grid)
library(ggpubr)
library(dplyr)

#Load the data
ChiTest.data <- read.csv("C:\\Users\\Seroney\\Downloads\\sample-csv-file-for-testing (1).csv")
str(ChiTest.data)
view(ChiTest.data)

#Independent Chi-test
#Create a frequency table for the variables
Chisq_freq.table <- table(ChiTest.data$Segment, ChiTest.data$Discount.Band)
Chisq_freq.table

#Perform the independent chisq.test
Ind_Chisq_test <- chisq.test(Chisq_freq.table)
Ind_Chisq_test

Ind_Chisq_test$expected #Code to show the expected values

#Goodness of fit test
#Check the freq of the discount band
Chisq_discount_freq <- table(ChiTest.data$Discount.Band)
Chisq_discount_freq

#Check the proportion of the target band
proportion_chisq.data <- (Chisq_discount_freq/ sum(Chisq_discount_freq))
proportion_chisq.data
plot(proportion_chisq.data)

#Perform goodness of fit test
Chisq_gofit_test <- chisq.test( proportion_chisq.data,p = c(0.35, 0.25, 
                                     0.35, 0.05))
Chisq_gofit_test

#Plot the data
ggplot(ChiTest.data) + 
  aes(x = Discount.Band, fill = Segment) + 
  geom_bar() + 
  scale_fill_hue() + 
  theme_minimal() 

