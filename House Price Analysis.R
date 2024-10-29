
#Import libraries
library(ggplot2)

#load the dataset
HouseData <- read.csv("C:\\Users\\Seroney\\Downloads\\Compressed\\archive_4\\House_Rent_Dataset.csv")

#View the data 
head(HouseData)
tail(HouseData)

print(paste('Number of rows:', nrow(HouseData)))
print(paste('Number of columns:', ncol(HouseData)))

str(HouseData)
colnames(HouseData)
summary(HouseData)

#Check the unique cities
unique(HouseData$City)

#Check for nulls
sum(is.null(HouseData))

#Check for duplicates
sum(duplicated(HouseData))

#Check the Mean, Median, Maximum & Minimum House Rents
print(paste('Mean house rent:', round(mean(HouseData$Rent))))
print(paste('Mean house rent:', round(median(HouseData$Rent))))
print(paste('Mean house rent:', round(max(HouseData$Rent))))
print(paste('Mean house rent:', round(min(HouseData$Rent))))

#Top five house rents
top5rents <- HouseData$Rent[order(HouseData$Rent, decreasing = TRUE)][1:5]
print(top5rents)

#Bar Plot for Number of House in Each City which is Available for Rent
ggplot(data = HouseData, aes(x = City)) + geom_bar(fill='blue') +
  labs(title = "Number of Houses Available for Rent in Each City",
       x = "City",
       y = "Number of Houses") +
  theme_minimal(base_size = 16) +  # Font scaling for better readability
  theme(axis.text.x = element_text(angle = 0))

#Bar plot to check the furnishing status
ggplot(data= HouseData, aes(x = factor(`Furnishing.Status`))) +
  geom_bar(fill = "orange") +
  labs(title = "Count of Different Furnishing Statuses",
       x = "Furnishing Status",
       y = "Count") +
  theme_minimal(base_size = 16) +  # Font scaling for readability
  theme(axis.text.x = element_text(angle = 0))

#Bar plot on the different types of tenants preferred
ggplot(data = HouseData, aes(x=Tenant.Preferred)) + geom_bar(fill="orange")+
  labs(title="Count of Tenants Preferred",
       x="Tenants",
       y="Number of Tenants") +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 0))

#Scatter plot of house size vs house rent
plot(HouseData$Size, HouseData$Rent, 
     main="Scatterplot of House Rent Vs House Size",
     xlab = "House Size",
     ylab = "House Rent",
     col="navyblue", pch=16)

#Scatter plot of house rent vs house size
plot(HouseData$Rent, HouseData$Size, 
     main="Scatterplot of House Rent Vs House Size",
     xlab = "House Size",
     ylab = "House Rent",
     col="skyblue", pch=16,
     cex = HouseData$Size / max(HouseData$Size) * 3)

# Load ggplot2 package
library(ggplot2)

# Scatter plot with ggplot2
ggplot(HouseData, aes(x = Rent, y = Size, size = Size)) +
  geom_point(color = "skyblue", shape = 16) +
  labs(title = "Scatterplot of House Rent vs House Size",
       x = "House Rent (sq ft)",
       y = "House Size ($)") +
  scale_size_continuous(range = c(1, 6)) +  # Adjust point size range
  theme_minimal()

#Bar plot of city vs Rent
#Import the scales library because the rent prices arre in millions
library(scales)

# Custom function to add "mil" suffix
label_millions <- function(x) {
  paste0(comma(x / 1e6), " mil")
}

# Bar plot with y-axis labels in "mil"
ggplot(HouseData, aes(x = City, y = Rent, fill = City)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_millions) +  # Use custom y-axis label function
  labs(title = "Rent in Every City",
       x = "City",
       y = "Rent ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Histogram of house sizes
ggplot(HouseData, aes(x = Size)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +  # Adjust binwidth as needed
  labs(title = "Distribution of House Sizes",
       x = "House Size (sq ft)",
       y = "Frequency") +
  theme_minimal()

