#packages
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
install.packages("corrgram")
library(corrgram)
#Reading CSV to a DF
bike <- read.csv('bikeshare.csv')

head(bike)

# scatter plot of count vs temp. 
ggplot(bike, aes(temp, count, color = temp)) + geom_point(alpha = 1/10) +
  scale_color_gradient(low = "dodgerblue1", high = "dodgerblue4", name = "Temperature") +
  theme_minimal()

# count versus datetime as a scatterplot with a color gradient based on temperature.
bike$date <- as.POSIXct(bike$datetime, format = "%Y-%m-%d")
ggplot(bike, aes(date, count, color = temp)) + geom_point(alpha = 0.4) +
  scale_color_gradient(low = "lightgreen", high = "tomato2", name = "Temperature") +
  theme_minimal()

# What is the correlation between temp and count?
cor_matrix <- cor(bike[, c("temp", "count")])
print(cor_matrix)


