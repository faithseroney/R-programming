#We will use Chronbach's alpha test to test for reliability

mytestdata2 <- read.csv("C:/Users/Seroney/Downloads/dataset-62794.csv")

head(mytestdata2)

str(mytestdata2)

alpha(mytestdata2, check.keys = TRUE)

reliability(cov(mytestdata2)) 
