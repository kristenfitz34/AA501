# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# June 25, 2024
################################################################################

################################################################################
# Introduction to Statistics - Day 1
# Describing Distributions Part 2: Visualization
################################################################################

# install packages
library(AmesHousing)
library(ggplot2)

# used only for this data - for ordinal variables
ames <- make_ordinal_ames()

# Creating a histogram
ggplot(data = ames) +
  geom_histogram(mapping = aes(x = Sale_Price/1000)) +
  labs(x = "Sales Price (Thousands $)") # labels x-axis

# Overlaying a density plot
ggplot(ames,aes(x=Sale_Price/1000)) +
  # makes y-axis to be density (needs two dots), alpha is shading darkness
  geom_histogram(aes(y=..density..), alpha=0.5) + 
  # adds density line, alpha is thickness of line
  geom_density( alpha = 0.2) +
  # labels x-axis
  labs(x = "Sales Price (Thousands $)")

# QQ-Plot
ggplot(data = ames, aes(sample = Sale_Price/1000)) +
  #all that is needed to plot dots
  stat_qq() +
  # plots the line
  stat_qq_line()

# Box plot
ggplot(ames, aes(y = Sale_Price/1000, x = Central_Air, 
                 # fill applies different colors for levels of Central_Air
                 fill = Central_Air)) + 
  geom_boxplot() + 
  # Labels axis
  labs(y = "Sales Price (Thousands $)", x = "Central Air") +
  # applies a palette to fill=
  scale_fill_brewer(palette="Accent") + 
  # theme_classic() changes background to white 
  # coord_flip() flips the axis
  theme_classic() + coord_flip()