#Logistic regression is performed on 1 dependent variable and 1+ nominal,ordinal\
#interval or ratio-level data

install.packages("aod")
install.packages("mlogit") 
install.packages("nnet") 
installed.packages("MASS") 
install.packages("rms") 
install.packages("DescTools") 
install.packages("manipulate") 
install.packages("reshape2")
install.packages("arm") 
install.packages("broom") 
install.packages("effects") 

library(aod) 
library(mlogit) 
library(nnet) 
library(MASS) 
library(rms) 
library(DescTools) 
library(manipulate) 
library(reshape2) 
library(ggplot2) 
library(arm) 
library(broom) 
library(dplyr) 
library(effects)

library(ggpubr)
library(dplyr)

LogNomData <- read.csv("C:/Users/Seroney/Downloads/VehChoicePrice.csv")
view(LogNomData)
str(LogNomData)

LogBinomData <- read.csv("C:\\Users\\Seroney\\Downloads\\TypeofCoverage.csv")
view(LogBinomData)

#Check the rship the type of coverage users get with age, gender, etc
BinomLRModel <- glm(urban~ age + seniority + men + marital_S, data = LogBinomData, 
               family = "binomial")
BinomLRModel
summary(BinomLRModel)

#Multinom: rship the type of vehicle has with age, urban, prices
MultNomLRModel <- multinom(veh ~ age + men + price.C + price.M + price.F, 
                           data = LogNomData, maxit=1e4)
summary(MultNomLRModel)

z<- summary(MultNomLRModel)$Coefficients/ summary(MultNomLRModel)$Std.Errors
z

pValues <- (1- pnorm(abs(z),0,1)) * 2
pValues

#Ordinal Logistic Regression
# first, factor dependent variable (var = yard) to ordinal data 
LogBinomData$yord <- factor(LogBinomData$yord, levels = 0:2, labels = 
                              c("unlikely", "Somewhat Likely", "Very Likely"))

OrdinalLRModel <- polr(yord ~ age + seniority + men + marital_S, 
                       data = LogBinomData, Hess=TRUE)
summary(OrdinalLRModel)

#Calculate and store p values
ctable <- coef(summary(OrdinalLRModel))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) *2
ctable<- cbind(ctable,"p value" = p)

view(ctable)

#Let's plot the binomial regression model:
coefplot(BinomLRModel)

#Multinomial regression plot
MN.LGraph <- broom::tidy(MultNomLRModel,conf.int=TRUE) 
MN.LGraph <- dplyr::filter(MN.LGraph, term!="(Intercept)") 
ggplot(MN.LGraph, aes(x=estimate, y=term, colour=y.level)) + 
  geom_point() + stat_smooth() + 
  labs(x = "Estimate", 
       y = "Coefficient", 
       title = "Multinominal Linearity plot") 
