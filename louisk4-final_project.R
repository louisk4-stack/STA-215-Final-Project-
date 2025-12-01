library(readr)
library(psych)
library(dplyr)

setwd("/courses/STA145/louisk4")

data <- read_delim("data-4.csv")

data_complete <- data %>%
  filter(complete.cases(.))

describe(data_complete)

#create scatterplot
linear_plot <- plot(data_complete$college_graduate, data_complete$unemployment_rate)
meany <- mean(data_complete$college_graduate)
meanx <- mean(data_complete$unemployment_rate)

abline(v = meany, col = "black")
abline(h = meanx, col = "black")

#create linear regression
linear_relationship <- lm(unemployment_rate ~ college_graduate, data = data)
summary(linear_relationship)

abline(linear_relationship, col = "red")

#create residuals
plot(data_complete$college_graduate, residuals(linear_relationship))