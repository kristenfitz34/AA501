# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# June 27, 2024
################################################################################

################################################################################
# Introduction to ANOVA & Regression - Day 3
# ANOVA
################################################################################

# packages
library(ggplot2)
library(car)
library(stats)
library(ggpubr)
# Reading data
cars2 <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/cars2.csv")

# testing normality
model<-aov(MPG~Country, data=cars2)

shapiro.test(residuals(model))

# Testing for equal variances
leveneTest(MPG~Country,data=cars2)

# Residuals vs. Fitted Plot
ggplot(model, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot',
       x='Fitted Values', y='Residuals')

# Welch's one-way ANOVA
oneway.test(MPG~Country, data = cars2,
            var.equal = FALSE)

# ANOVA as regression
cars_lm <- lm(MPG ~ factor(Country), data = cars2)
summary(cars_lm)

# ANOVA Predictions
cars2$predict <- predict(cars_lm, data = cars2)
cars2$resid_anova <- resid(cars_lm, data = cars2)
model_output<-cars2 %>% dplyr::select(Country, predict,
                                      resid_anova)
rbind(model_output[1:3,],model_output[255:257,],
      model_output[375:377,])

################################################################################
# When normality fails
################################################################################

ggdensity(train, x = "Sale_Price",
          add = "mean", rug = TRUE,
          color = "Exter_Qual", fill = "Exter_Qual")
fligner.test(Sale_Price ~ Exter_Qual, data =
               train) # DOES NOT depend on Normality

################################################################################
# Post hoc test
################################################################################

# Tukey's test
cars_aov <- aov(MPG ~ factor(Country), data = cars2)
gh.cars <- TukeyHSD(cars_aov)
print(tukey.cars)
par(mar=c(4,10,4,2))
plot(tukey.cars, las=1)

# Games-Howell Test 
install.packages("PMCMRplus")
library(PMCMRplus)
cars_aov <- aov(MPG ~ factor(Coutry),data=cars2)
gh.cars <- gamesHowellTest(cars_aov)
summary(gh.cars)

# Dunn's test
library(dunn.test)
dunn.cars<- dunn.test(cars2$MPG,cars2$Country,kw=T,method="bonferroni")

# Dunnett's test 
cars2$group<-ifelse(cars2$Country=="US","aUS",cars2$Country)
dunnettTest(x = cars2$MPG, g = factor(cars2$group))

# Welch and Wilcoxon with Bonferroni adjustment
summary(welchManyOneTTest(x = cars2$MPG, g = cars2$Country, p.adjust.method
                          = "bonferroni"))

summary(manyOneUTest(x = cars2$MPG, g = cars2$Country, p.adjust.method =
                       "bonferroni"))