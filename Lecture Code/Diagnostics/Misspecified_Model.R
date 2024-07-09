# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# July 8, 2024
################################################################################

################################################################################
# Diagnostics - Day 8
# Diagnostics - Miss specified Model
################################################################################

# Loading packages
library(tidyverse)
library(ggplot2)

# Loading Data
amount=c(1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5)
strength=c(2.4,2.6,2.7,2.5,2.6,2.6,2.7,2.8,2.8,2.8,3.0,3.0,3.0,2.9,
           2.9,3.0,3.1,2.9,2.9,3.0,2.9,2.8)

# Linear Regression Residual Plot
lm.quad=lm(strength~amount)
summary(lm.quad)

ggplot(lm.quad, aes(x=amount,y=resid(lm.quad))) +
  geom_point(color="blue",size=3) + 
  labs(title="Residual plot", x="Amount", y="Residuals")

# Need to turn it to a quadratic 
amount.c <- scale(amount, scale=F)
lm.quad2 = lm(strength ~ amount.c + I(amount.c^2))
summary(lm.quad2)

ggplot(lm.quad2, aes(x=amount, y=resid(lm.quad2))) + 
  geom_point(color="blue",size=3)+
  labs(title="Residual plot", x="Amount", y="Residuals")

# Third Degree Polynomial 
lm.quad3 = lm(strength ~ amount.c + I(amount.c^2) + I(amount.c^3))
summary(lm.quad3)

ggplot(lm.quad3, aes(x=amount, y=resid(lm.quad3))) +
  geom_point(color="orange",size=2) +
  labs(title="Residual plot", x="Amount", y="Residuals")