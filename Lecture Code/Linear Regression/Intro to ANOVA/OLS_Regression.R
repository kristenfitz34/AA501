# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# June 28, 2024
################################################################################

################################################################################
# Introduction to ANOVA & Regression - Day 4
# Ordinary Least Squares (OLS) Regression 
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

# Test of correlation
cor.test(train$Gr_Liv_Area,train$Sale_Price)

# Correlation Matrix
cor(train[, c('Year_Built','Total_Bsmt_SF','First_Flr_SF',
              'Gr_Liv_Area','Sale_Price')])

# Plot matrix
pairs(train[, c('Year_Built','Total_Bsmt_SF','First_Flr_SF',
                'Gr_Liv_Area','Sale_Price')])

# Simple Linear Regression
slr <- lm(Sale_Price ~ Gr_Liv_Area, data=train)
par(mfrow=c(2,2))
plot(slr)

summary(slr)

# SLR with categorical
slr <- lm(Sale_Price ~ factor(Central_Air), data=train)
par(mfrow=c(2,2)) 
plot(slr)

summary(slr)
