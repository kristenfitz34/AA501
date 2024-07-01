# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# July 1, 2024
################################################################################

################################################################################
# More Complex ANOVA & Regression - Day 5
# Two-way ANOVA
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

# Exploring the Data
new_train <- train %>%
  group_by(Heating_QC, Central_Air) %>%
  summarise(mean = mean(Sale_Price),
            sd = sd(Sale_Price),
            max = max(Sale_Price),
            min = min(Sale_Price),
            n = n())

# Two-way ANOVA
ames_aov2 <- aov(Sale_Price ~ Heating_QC + Central_Air, data = train)
summary(ames_aov2)

# Post-Hoc Testing
tukey.ames2 <- TukeyHSD(ames_aov2)
print(tukey.ames2)
plot(tukey.ames2)

# Interactions
ames_aov_int <- aov(Sale_Price ~ Heating_QC*Central_Air, data = train)
summary(ames_aov_int)

# Post-hoc testing
tukey.ames_int <- TukeyHSD(ames_aov_int)
plot(tukey.ames_int, las = 1)

# Sliced ANOVA
CA_aov <- train %>%
  group_by(Central_Air) %>%
  nest() %>%
  mutate(aov = map(data, ~summary(aov(Sale_Price ~ Heating_QC, data = .x))))
print(CA_aov$aov)
