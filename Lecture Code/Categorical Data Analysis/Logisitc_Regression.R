# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# July 16, 2024
################################################################################

################################################################################
# Categorical Data Analysis - Day 13
# Logistic Regression
################################################################################

# Loading packages
library(tidyverse)
library(AmesHousing)
library(survival)

# loading data
ames <- make_ordinal_ames()

# Traing-Test Split in R
set.seed(123)
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')

train <- train %>%
  mutate(Bonus = ifelse(Sale_Price > 175000, 1, 0))

# binary logistic regression
ames_logit <- glm(Bonus ~ Gr_Liv_Area, data = train,
                  family = binomial(link = "logit"))

summary(ames_logit)

# Odds Ratio and confidence interval for the log odds of the coefficients 
100*(exp(cbind(coef(ames_logit), confint(ames_logit)))-1)

# odds ratio 
ames_logit2 <- glm(Bonus ~ Gr_Liv_Area + Central_Air + factor(Fireplaces),
                   data = train, family = binomial(link = "logit"))

100*(exp(cbind(coef(ames_logit2), confint(ames_logit2)))-1)

# Concordance
survival::concordance(ames_logit)

# Forward Selection
train_sel_log <- train %>%
  dplyr::select(Bonus, Lot_Area, Street, Bldg_Type, House_Style,
                Overall_Qual, Roof_Style, Central_Air,
                First_Flr_SF, Second_Flr_SF, Full_Bath, Half_Bath,
                Fireplaces, Garage_Area, Gr_Liv_Area,
                TotRms_AbvGrd) %>%
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
full.model <- glm(Bonus ~ . , data = train_sel_log)
empty.model <- glm(Bonus ~ 1, data = train_sel_log)

# Backward Selection
for.model <- step(empty.model,
                  scope = list(lower = formula(empty.model),
                               upper = formula(full.model)),
                  direction = "forward",
                  k = log(dim(train_sel_log)[1])) #BIC

back.model <- step(full.model,
                   scope = list(lower = formula(empty.model),
                                upper = formula(full.model)),
                   direction = "backward",
                   k = log(dim(train_sel_log)[1])) #BIC