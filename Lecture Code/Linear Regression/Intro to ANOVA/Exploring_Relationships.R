# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# June 27, 2024
################################################################################

################################################################################
# Introduction to ANOVA & Regression - Day 3
# Exploring Relationships
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

# Scatter plots for checking relationships
ggplot(data = train) +
  geom_point(mapping = aes(x = Gr_Liv_Area, y = Sale_Price/1000)) +
  labs(y = "Sales Price (Thousands $)", x = "Greater Living Area
(Sqft)")

# Grouped box-plots 
ggplot(data = train, aes(y = Sale_Price/1000,
                         x = Exter_Qual,
                         fill = Exter_Qual)) +
  geom_boxplot() +
  labs(y = "Sales Price (Thousands $)",
       x = "Exterior Quality Category") +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "red",
               fill = "red") +
  scale_fill_brewer(palette="Blues") +
  theme_classic() + coord_flip()

# Overlaid Histograms
ggplot(ames,aes(x = Sale_Price/1000,
                fill = Exter_Qual)) +
  geom_density(alpha = 0.2,
               position = "identity") +
labs(x = "Sales Price (Thousands $)")
