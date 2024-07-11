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

# Training data
train_reg <- train %>%
  select(Sale_Price, Lot_Area, Street,
         Bldg_Type, House_Style, Overall_Qual,
         Roof_Style, Central_Air, First_Flr_SF,
         Second_Flr_SF, Full_Bath, Half_Bath,
         Fireplaces, Garage_Area, Gr_Liv_Area,
         TotRms_AbvGrd) %>%
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))

# Training Data 
test_reg <- test %>%
  select(Sale_Price, Lot_Area, Street,
         Bldg_Type, House_Style, Overall_Qual,
         Roof_Style, Central_Air, First_Flr_SF,
         Second_Flr_SF, Full_Bath, Half_Bath,
         Fireplaces, Garage_Area, Gr_Liv_Area,
         TotRms_AbvGrd) %>%
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))

########################### Ridge Regression ###################################

# Must you matrix approach for regularized regression
# Giving x-variables as single matrix
# model.matrix takes categorical variables and dummy encodes into separate columns
train_x <- model.matrix(Sale_Price ~ ., data = train_reg)[, -1] # removes column 1 (intercept)

train_y <- train_reg$Sale_Price

test_x <- model.matrix(Sale_Price ~ ., data = test_reg)[, -1]
test_y <- test_reg$Sale_Price


# Ridge Regression
ames_ridge <- glmnet(x = train_x, y = train_y, alpha = 0) # alpha = 0 for ridge regression
plot(ames_ridge, xvar = "lambda")

########################### LASSO Regression ###################################
# Ridge Regression
ames_lasso <- glmnet(x = train_x, y = train_y, alpha = 1) # alpha = 1 for LASSO regression

# x-axis at top of graph is the number of variables you have left in the model
plot(ames_lasso, xvar = "lambda")

########################## Elastic Net Regression ##############################

ames_en <- glmnet(x = train_x, y = train_y, alpha = 0.5)
plot(ames_en, xvar = "lambda")

######################## Optimizing Penalties ##################################

# LASSO
# Use cv.glmnet - better for selecting lambda using CV on regularized regression
# This should be the first function you start with
# glmnet() is for building your final model
ames_lasso_cv <- cv.glmnet(x = train_x, y = train_y, alpha = 1)

plot(ames_lasso_cv)

plot(ames_lasso, xvar = "lambda")
abline(v = log(ames_lasso_cv$lambda.1se), col = "red", lty = "dashed")
abline(v = log(ames_lasso_cv$lambda.min), col = "black", lty = "dashed")

coef(ames_lasso, s = c(ames_lasso_cv$lambda.min, ames_lasso_cv$lambda.1se))

#####################3####### Predictions ######################################

test$pred_lm <- predict(ames_lm, newdata = test)
head(test$pred_lm)

test_reg$pred_lasso <- predict(ames_lasso, s = ames_lasso_cv$lambda.1se,
                               newx = test_x)
head(test_reg$pred_lasso)

test %>%
  mutate(lm_APE = 100*abs((Sale_Price - pred_lm)/Sale_Price)) %>%
  dplyr::summarise(MAPE_lm = mean(lm_APE))

test_reg %>%
  mutate(lasso_APE = 100*abs((Sale_Price - pred_lasso)/Sale_Price)) %>%
  dplyr::summarise(MAPE_lasso = mean(lasso_APE))
