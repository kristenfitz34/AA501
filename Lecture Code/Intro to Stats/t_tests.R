# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# June 26, 2024
################################################################################

################################################################################
# Introduction to Statistics - Day 2
# Two Sample t-tests
################################################################################

# libraries & data
library(ggplot2)
cars <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/cars.csv")

# Verifying Normality with a QQ-Plot
ggplot(cars, aes(sample = MPG, color = Country)) +
  stat_qq() + 
  stat_qq_line()

# Shapiro-Wilk’s test - null hypothesis is that the data is normal 
shapiro.test(cars$MPG[cars$Country == "US"])
shapiro.test(cars$MPG[cars$Country == "Japanese"])

# Testing if the variances are equal or unequal
var.test(MPG ~ Country, data = cars) # rejecting to be conservative, unequal var

# Performing two sided t-test with unequal variance
# First variance in equation is the numeric variable you are 
# testing the means for.
t.test(MPG ~ Country, data=cars, var.equal = FALSE)

# Conclusion: there is a significant difference that the average MPG differs
# between the US and Japan.
################################################################################

################################################################################
# two sample t-test without normality using Wilcoxon rank test

library(AmesHousing)
ames <- make_ordinal_ames()

ggplot(ames, aes(sample = Sale_Price, color = Central_Air)) +
  stat_qq() +
  stat_qq_line()

# Wilcoxon rank test - null hypothesis: the samples are equal to 0 (symmetric)
# reject null data is not symmetric
wilcox.test(Sale_Price ~ Central_Air, data = ames)