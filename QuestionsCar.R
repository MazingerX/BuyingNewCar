library(ISLR)
library(tidyverse)
library(car)
library(dplyr)


str(Auto)
View(Auto)
# Use Q-Q Plot and Shapiro-Wilk test to make sure is normal

data1 <- rnorm(1000)
hist(data1, main = "Histogram of data", xlab = "Data Values")

qqnorm(data1)

shapiro.test(data1)

data_hp_acce <- select(Auto, horsepower, acceleration)
ggplot(data_hp_acce, aes(horsepower, acceleration)) + geom_point() + stat_smooth(method = lm)
