library(tidyverse)
library(ggpubr)
library(GGally)

MyRegData <- readxl::read_excel("C:/Users/Seroney/Downloads/case fit regression model.xlsx")

view(MyRegData)
str(MyRegData)

#Lets perform linear regression
simp_LinRegression <- lm(Viscosity ~ Temp, data = MyRegData)
simp_LinRegression
summary(simp_LinRegression)

Mult_LinReg <- lm(Viscosity ~ Temp + Stir + Time,  data = MyRegData)
Mult_LinReg
summary(Mult_LinReg)

plot(Viscosity ~ Temp, data=MyRegData, main="Simple Linearity")
abline(simp_LinRegression, col="blue")

ggplot(simp_LinRegression, aes(x = Temp, y =Viscosity )) + geom_point() + 
  stat_smooth() 

plot(Mult_LinReg)
ggpairs(MyRegData[c("Viscosity", "Temp", "Stir", "Time")])

