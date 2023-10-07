library(ISLR)
library(tidyverse)
library(car)
library(dplyr)
library(ggplot2)


str(Auto)
View(Auto)
# Use Q-Q Plot and Shapiro-Wilk test to make sure is normal

pairs(Auto)

data1 <- rnorm(1000)
hist(data1, main = "Histogram of data", xlab = "Data Values")

qqnorm(data1)

shapiro.test(data1)

library(ggplot2)
ggplot(Auto, aes(x = horsepower, y = acceleration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Horsepower and Acceleration",
       x = "Horsepower",
       y = "Acceleration")


cor(Auto$horsepower, Auto$acceleration)

data_hp_acce1 <- rnorm(1000)
qqnorm(data_hp_acce1)
shapiro.test(data_hp_acce1)
