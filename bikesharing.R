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

# boxplot with the y axis indicating count and the x axis begin a box for each season.
bike$season <- as.factor(bike$season)
ggplot(bike, aes(season, count, color = season)) + geom_boxplot()

#Creating an "hour" column that takes the hour from the datetime column. 
#Applying function to the entire datetime column and reassign it.

# Install and load the lubridate package if not already installed
install.packages("lubridate")
library(lubridate)

# Assuming 'bike' is your data frame with 'datetime' column
bike$hour <- as.factor(hour(bike$datetime))

# scatterplot of count versus hour, with color scale based on temp. Using only bike data where workingday == 1
# Filter data where workingday == 1
library(dplyr)
bike_workingdays <- bike %>% filter(workingday == 1)

# Create custom gradient colors
custom_colors <- c('blue', 'cyan', 'green', 'yellow', 'orange', 'red')

# Create scatterplot with custom gradient colors and jitter
ggplot(bike_workingdays, aes(x = hour, y = count, color = temp)) +
  geom_point(position = position_jitter(w = 1, h = 0), alpha = 0.7) +
  scale_color_gradientn(colors = custom_colors, name = "Temperature") +
  labs(title = "Scatterplot of Count vs. Hour", x = "Hour", y = "Count")
  +scale_x_continuous(breaks = seq(0, 23, by = 1)) +  # Specify breaks for each hour
  +theme_minimal()  # You can customize the theme if needed

#same plot for non working days
bike_nonworkingdays <- bike %>% filter(workingday == 0)

ggplot(bike_nonworkingdays, aes(x = hour, y = count, color = temp)) +
  geom_point(position = position_jitter(w = 1, h = 0), alpha = 0.7) +
  scale_color_gradientn(colors = custom_colors, name = "Temperature") +
  labs(title = "Scatterplot of Count vs. Hour", x = "Hour", y = "Count")
  +scale_x_continuous(breaks = seq(0, 23, by = 1)) +  # Specify breaks for each hour
  +theme_minimal()  # You can customize the theme if needed

# Using lm() to build a model that predicts count based solely on the temp feature

temp.model <- lm(count ~temp, data = bike)

summary(temp.model)

#How many bike rentals would we predict if the temperature was 25 degrees Celsius?

pred <- predict(temp.model, data.frame(temp = 25))
pred


bike$hour_num <- as.numeric(as.character(bike$hour))
bike$season <- as.factor(bike$season)

columns_model <- c("season", "holiday", "workingday", "weather", "temp", "humidity", "windspeed", "hour")

bike$hour <- hour(bike$datetime)

pred_multi <- lm(count ~ . - casual - registered - datetime - atemp, data = bike)

summary(pred_multi)
