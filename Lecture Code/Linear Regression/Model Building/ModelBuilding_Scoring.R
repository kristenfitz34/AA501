# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# July 11, 2024
################################################################################

################################################################################
# Model Building & Scoring Prediction - Day 11
# Regular Regression
################################################################################

# Loading packages
library(tidyverse)
library(AmesHousing)
library(ggplot2)
library(glmnet)

# loading data 
ames <- make_ordinal_ames()

# Traing-Test Split in R
set.seed(123)
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')

########################### Ridge Regression ###################################
train_reg <- train %>%
  select(Sale_Price, Lot_Area, Street,
         Bldg_Type, House_Style, Overall_Qual,
         Roof_Style, Central_Air, First_Flr_SF,
         Second_Flr_SF, Full_Bath, Half_Bath,
         Fireplaces, Garage_Area, Gr_Liv_Area,
         TotRms_AbvGrd) %>%
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))

# Must you matrix approach for regularized regression
# Giving x-variables as single matrix
# model.matrix takes categorical variables and dummy encodes into separate columns
train_x <- model.matrix(Sale_Price ~ ., data = train_reg)[, -1] # removes column 1 (intercept)

train_y <- train_reg$Sale_Price

ames_ridge <- glmnet(x = train_x, y = train_y, alpha = 0)
plot(ames_ridge, xvar = "lambda")