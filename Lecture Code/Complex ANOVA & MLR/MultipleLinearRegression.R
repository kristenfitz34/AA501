# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# July 3, 2024
################################################################################

################################################################################
# More Complex ANOVA & Regression - Day 6
# Multiple Linear Regression
################################################################################

# Loading packages
library(tidyverse)
library(AmesHousing)
library(ggplot2)

# loading data 
ames <- make_ordinal_ames()

# Traing-Test Split in R
set.seed(123)
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')

# Multiple linear regression
ames_lm2 <- lm(Sale_Price ~ Gr_Liv_Area + TotRms_AbvGrd, data = train)
summary(ames_lm2)

# Plotting for assumptions
# violates constant variance and normality
par(mfrow = c(2, 2))
plot(ames_lm2)

# MLR with categorical variables - Central Air 
ames_lm3 <- lm(Sale_Price ~ Gr_Liv_Area + TotRms_AbvGrd + Central_Air, data = train)
summary(ames_lm3)