# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# July 5, 2024
################################################################################

################################################################################
# Model Selection - Day 7 
# Model Selection
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

# Selecting some variables - trashing unimportant variables
train_sel <- train %>%
  select(Sale_Price, Lot_Area, Street,
                Bldg_Type, House_Style, Overall_Qual,
                Roof_Style, Central_Air, First_Flr_SF,
                Second_Flr_SF, Full_Bath, Half_Bath,
                Fireplaces, Garage_Area, Gr_Liv_Area,
                TotRms_AbvGrd) %>%
# Replacing all missing with mean of the column for now.
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))

################################################################################
# Forward Selection - AIC
# step model looks for 4 inputs 

# starting model, most start with zero variables - intercept only model
empty.model <- lm(Sale_Price ~ 1, data = train_sel) 

# Upper model, starts with all variables 
full.model <- lm(Sale_Price ~ ., data = train_sel) 

# start with empty model
for.model <- step(empty.model,
                  # stepping between (smallest model)
                  scope = list(lower = empty.model, 
                  # bigger model stepping between
                               upper = full.model),
                  # Direction = forward for forward selection
                  direction = "forward", k = 2) # k = 2 (AIC)

################################################################################
# Forward Selection - BIC (output will still say AIC)
for.model <- step(empty.model,
                  scope = list(lower = empty.model,
                               upper = full.model),
                  direction = "forward", k = log(nrow(train_sel))) # k = BIC

################################################################################
# Forward Selection - p-value selection 

for.model <- step(empty.model,
                  scope = list(lower = empty.model,
                               upper = full.model),
                  direction = "forward", 
                  k = qchisq(0.05, 1, lower.tail = FALSE)) # p.value selection

################################################################################
# Backward Selection - AIC

full.model <- lm(Sale_Price ~ ., data = train_sel)
empty.model <- lm(Sale_Price ~ 1, data = train_sel)

# Start with full model
back.model <- step(full.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "backward", k = 2) 

################################################################################
# Backward Selection - BIC
back.model <- step(full.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "backward", k = log(nrow(train_sel)))

################################################################################
# Backward Selection - p-value
back.model <- step(full.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "backward",
                   k = qchisq(0.05, 1, lower.tail = FALSE)) 

################################################################################
# Stepwise Selection - AIC
step.model <- step(empty.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "both", k = 2) 

################################################################################
# Stepwise Selection - BIC
step.model <- step(empty.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "both", k = log(nrow(train_sel)))

################################################################################
# Stepwise Selection - p-value
step.model <- step(empty.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "both", k = qchisq(0.05, 1, lower.tail = FALSE))

