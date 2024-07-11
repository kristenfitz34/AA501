# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# July 8, 2024
################################################################################

################################################################################
# Diagnostics - Day 8
# Diagnostics - Non-Constant Variance
################################################################################

# Loading packages
library(tidyverse)
library(ggplot2)

# Loading Data

years=c(1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,
        9,10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,
        15,16,16,16,16,17,17,17,17,17,18,18,18,18,19,19,19,19,20,20,20,20,21,21
        ,21,21,22,22,22,22,23,23,23,23,24,24,24,24,25,25,25)

salary=c(41504,32619,44322,40038,46147,38447,38163,42104,25597,39599,55698,
         47220,65929,55794,45959,52460,60308,61458,56951,56174,59363,57642,
         69792,59321,66379,64282,48901,100711,59324,54752,73619,65382,58823,
         65717,92816,72550,71365,88888,62969,45298,111292,91491,106345,99009,
         73981,72547,74991,139249,119948,128962,98112,97159,125246,89694,73333,
         108710,97567,90359,119806,101343,147406,153020,143200,97327,184807,
         146263,127925,159785,174822,177610,210984,160044,137044,182996,184183,
         168666,121350,193627,142611,170131,134140,129446,201469,202104,220556,
         166419,149044,247017,247730,252917,235517,241276,197229,175879,253682,
         262578,207715,221179,212028,312549)

lm.salary = lm(salary ~ years)

ggplot(lm.salary, aes(x=fitted(lm.salary),y=resid(lm.salary))) +
  geom_point(color="blue") +
  labs(title="Residual Plot", x="Predicted Values",y="Residuals")

# Spearman Rank Correlation Test
cor.test(abs(resid(lm.salary)), fitted.values(lm.salary), 
         method="spearman", exact=T)

# Use variance-stabilizing transformation - log transformation
lm.salary.log = lm(log(salary) ~ years)

ggplot(lm.salary.log, aes(x=fitted(lm.salary.log), y=resid(lm.salary.log))) +
  geom_point(color = "orange")+
  labs(title="Residual Plot", x="Predicted Values",y="Residuals")


########################## Weighted Least Squares ##############################


# Weighted Least Squares
x <- c(rep(2:10, each=5))
set.seed(26785)
error <- rnorm(length(x), 0, x)
y <- 50 + 1.2*x + error

model1 <-lm(y ~ x)

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point()

# WLS - compare results
summary(model1)
weights1 <- 1 / x^2

model2 <- lm(y ~ x, weights = weights1)
summary(model2)

weights2 <- 1 / model1$fitted.values^2

model3 <- lm(y ~ x, weights=weights2)
summary(model3)

########################## Robust Standard Error ###############################

# HC1 
model3 <- lm_robust(y ~ x, se_type = "HC1")
summary(model3)

# HC2
model4 <- lm_robust(y ~ x)
summary(model4)