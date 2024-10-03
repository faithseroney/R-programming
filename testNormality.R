install.packages("dplyr")
install.packages("ggpubr")

library(dplyr)
library(ggpubr)

MyTestData <- read.csv("C:/Users/Seroney/Downloads/sample-csv-file-for-testing.csv")

head(MyTestData)

ks.test(MyTestData$Units.Sold, "pnorm")
ks.test(MyTestData$Month.Number, "pnorm")

shapiro.test(MyTestData$Units.Sold)
shapiro.test(MyTestData$Month.Number)

str(MyTestData)

#Convert to factors then to numeric
MyTestData$Segment <- as.factor(MyTestData$Segment)
MyTestData$Segment <-as.numeric(MyTestData$Segment)

MyTestData$Country <-as.factor(MyTestData$Country)
MyTestData$Country <- as.numeric(MyTestData$Country)

MyTestData$Product <- as.factor(MyTestData$Product)
MyTestData$Product <- as.numeric(MyTestData$Product)

MyTestData$Discount.Band <- as.factor(MyTestData$Discount.Band)
MyTestData$Discount.Band <- as.numeric(MyTestData$Discount.Band)

MyTestData$Manufacturing.Price <- as.factor(MyTestData$Manufacturing.Price)
MyTestData$Manufacturing.Price <- as.numeric(MyTestData$Manufacturing.Price)

MyTestData$Sale.Price <- as.factor(MyTestData$Sale.Price)
MyTestData$Sale.Price <- as.numeric(MyTestData$Sale.Price)

MyTestData$Gross.Sales <- as.factor(MyTestData$Gross.Sales)
MyTestData$Gross.Sales <- as.numeric(MyTestData$Gross.Sales)

MyTestData$Discounts <- as.factor(MyTestData$Discounts)
MyTestData$Discounts <- as.numeric(MyTestData$Discounts)

MyTestData$Sales <- as.factor(MyTestData$Sales)
MyTestData$Sales <- as.numeric(MyTestData$Sales)

MyTestData$COGS <- as.factor(MyTestData$COGS)
MyTestData$COGS <- as.numeric(MyTestData$COGS)

MyTestData$Profit <- as.factor(MyTestData$Profit)
MyTestData$Profit <- as.numeric(MyTestData$Profit)

MyTestData$Date <- as.factor(MyTestData$Date)
MyTestData$Date <- as.numeric(MyTestData$Date)

MyTestData$Month.Name <- as.factor(MyTestData$Month.Name)
MyTestData$Month.Name <- as.numeric(MyTestData$Month.Name)

str(MyTestData)
View(MyTestData)

lapply(MyTestData, ks.test, "pnorm")
lapply(MyTestData, shapiro.test)

ggdensity(MyTestData$Sales, main = "Distribution of Sales",
                                    xlab = "Sales ($)")
ggdensity(MyTestData$Profit, main = "Distribution of Profit",
          xlab = "Profit ($)")

ggqqplot(MyTestData$Sales, main = "gplot Distribution of Sales", 
         xlab = "Sales ($)", ylab = "Marginal Mean") 
ggqqplot(MyTestData$Profit, main = "gplot Distribution of Profit", 
         xlab = "Sales ($)", ylab = "Marginal Mean") 

#Using Chronbachs Alpha Test
install.packages("psych") 
install.packages("umx") 
library(psych) 
library(umx) 