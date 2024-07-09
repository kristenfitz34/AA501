# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# July 9, 2024
################################################################################

################################################################################
# Diagnostics - Day 9
# Correlated Error Terms (Independence)
################################################################################

# loading packages
library(tidyverse)
library(TSA)

data(google)
x=seq(1,length(google))

# Durbin-Watson Test
lm.model = lm(google ~ x)
dwtest(lm.model, alternative="greater")