# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# June 26, 2024
################################################################################

################################################################################
# Categorical Data Analysis - Day 12
# Categorical Data Analysis
################################################################################

# Loading packages
library(tidyverse)
library(AmesHousing)
library(gmodels)
library(ggplot2)
library(vcdExtra)
library(DescTools)

# loading data
ames <- make_ordinal_ames()

# Traing-Test Split in R
set.seed(123)
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')

train <- train %>%
  mutate(Bonus = ifelse(Sale_Price > 175000, 1, 0))

# Exploring the data
table(train$Central_Air)

ggplot(data = train) +
  geom_bar(mapping =
             aes(x = Central_Air))

table(train$Bonus)
ggplot(data = train) +
  geom_bar(mapping =
             aes(x = Bonus))

# Cross-Tabulation Table (rows, columns)
table(train$Central_Air, train$Bonus)
ggplot(data = train) +
  geom_bar(mapping =
             aes(x = Bonus,
                 fill = Central_Air))

CrossTable(train$Central_Air, train$Bonus)

# Pearson Chi-Square Test - input is a table of variables
chisq.test(table(train$Central_Air, train$Bonus))

# Fisher's exact test
fisher.test(table(train$Central_Air, train$Bonus))

# Mantel-Haenszel Test - Ordinal Variables
CMHtest(table(train$Central_Air, train$Bonus))$table[1,]

# Odds Ratio - input is a table! 
# result - focuses on upper left hand column - Row 1 over Row 2
OddsRatio(table(train$Central_Air, train$Bonus))

# Cramer's V
assocstats(table(train$Central_Air, train$Bonus))

# Spearman's correlation
cor.test(x = as.numeric(ordered(train$Central_Air)),
         y = as.numeric(ordered(train$Bonus)),
         method = "spearman")
