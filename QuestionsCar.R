library(ISLR)
library(tidyverse)
library(car)
library(dplyr)
library(ggplot2)
library(vcd)


str(Auto)
View(Auto)
# Use Q-Q Plot and Shapiro-Wilk test to make sure is normal

pairs(Auto)

data1 <- rnorm(1000)
hist(data1, main = "Histogram of data", xlab = "Data Values")

qqnorm(data1)
shapiro.test(data1)

---
hp_acce <- select(Auto, horsepower, acceleration)
ggplot(hp_acce, aes(x = horsepower, y = acceleration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Horsepower and Acceleration",
       x = "Horsepower",
       y = "Acceleration")


cor(Auto$horsepower, Auto$acceleration)

---
mpg_cyl <- select(Auto, mpg, cylinders)
ggplot(mpg_cyl, aes(x = as.factor(cylinders), y = mpg)) +
  geom_boxplot() +
  labs(title = "Relationship between MPG and Cylinders",
       x = "Number of Cylinders",
       y = "MPG") +
  theme_minimal()

Auto %>%
  group_by(cylinders) %>%
  summarise(
    mean_mpg = mean(mpg),
    median_mpg = median(mpg),
    sd_mpg = sd(mpg),
    count = n()
  )

anova_result <- aov(mpg ~ as.factor(cylinders), data = Auto)
summary(anova_result)

---
table_cyl_origin <- table(Auto$cylinders, Auto$origin)
print(table_cyl_origin)
mosaicplot(table_cyl_origin, main="Mosaic Plot of Cylinders and Origin", xlab="Cylinders", ylab="Origin", col=c("orange", "pink", "red"))
chi_sq_test <- chisq.test(table_cyl_origin)
print(chi_sq_test)

ggplot(Auto, aes(x = displacement, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Displacement and Weight",
       x = "Displacement",
       y = "Weight") +
  theme_minimal()

cor(Auto$displacement, Auto$weight)

---
table_origin <- table(Auto$origin)
prop_origin <- prop.table(table_origin) * 100  # Convert to percentage
print(table_origin)
print(prop_origin)

barplot(prop_origin,
        main = "Percentage of Observations by Origin",
        xlab = "Origin",
        ylab = "Percentage",
        col = c("red", "green", "blue"),
        ylim = c(0, 100))

---


