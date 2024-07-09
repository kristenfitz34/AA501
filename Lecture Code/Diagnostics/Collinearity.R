# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# July 9, 2024
################################################################################

################################################################################
# Diagnostics - Day 9
# Collinearity 
################################################################################

library(car)

# correlation
cor(mtcars)

# VIF
lm.model = lm(mpg ~ ., data = mtcars)
vif(lm.model)
