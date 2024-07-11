# AA 501 - Analytics Foundations
# Kristen Fitzgerald
# July 9, 2024
################################################################################

################################################################################
# Diagnostics - Day 9
# Anomalous Observations
################################################################################

# Loading pacakges
library(ggplot2)

# Loading data

url = 'http://www.statsci.org/data/general/hills.txt'

races.table = read.table(url, header=TRUE, sep='\t')

n.index = seq(1, nrow(races.table))

races.table = cbind(races.table, n.index)
lm.model = lm(Time ~ Distance + Climb, data=races.table)

# Studentized Residuals 
ggplot(lm.model,  aes( x = n.index, y = rstudent(lm.model))) +
  geom_point(color="orange") + 
  geom_line(y = -3) +
  geom_line(y = 3) +
  labs(title = "External Studentized Residuals", x = "Observation",
       y = "Residuals")

# Cook's Distance
D.cut = 4 / (nrow(races.table) - 3)

ggplot(lm.model, aes( x = n.index, y = cooks.distance(lm.model))) +
  geom_point(color = "orange") +
  geom_line(y = D.cut) + 
  labs(title = "Cook's D", x = "Observation", y = "Cook's Distance")

# DFFITS
df.cut = 2*(sqrt( 3 / nrow(races.table)))

ggplot(lm.model, aes(x = n.index, y = dffits(lm.model))) + 
  geom_point(color="orange") + 
  geom_line(y = df.cut) +
  geom_line(y = -df.cut) +
  labs(title = "DFFITS", x="Observation", y="DFFITS")

db.cut = 2 / sqrt(nrow(races.table))

# Hat values
hat.cut = 2*(3) / nrow(races.table)

ggplot(lm.model, aes(x = n.index, y = hatvalues(lm.model))) +
  geom_point(color = "orange") +
  geom_line(y = hat.cut) +
  labs(title = "Hat values", x="Observation", y="Hat Values")

# DFBeta

ggplot(lm.model, aes(x = n.index, y = dfbetas(lm.model)[,'Climb'])) +
  geom_point(color="orange") + 
  geom_line(y = db.cut) +
  geom_line(y = -db.cut) +
  labs(title = "DFBETA for Climb", x = "Observation", y = "DFBETAS")

ggplot(lm.model, aes(x = n.index, y = dfbetas(lm.model)[,'Distance'])) +
  geom_point(color="orange") + 
  geom_line(y = db.cut) +
  geom_line(y = -db.cut) +
  labs(title = "DFBETA for Distance", x = "Observation", y = "DFBETAS")

n.index = seq(1, nrow(train))

ggplot(third.lm,  aes( x = n.index, y = rstudent(third.lm))) +
  geom_point(color="orange") + 
  geom_line(y = -3) +
  geom_line(y = 3) +
  labs(title = "External Studentized Residuals", x = "Observation",
       y = "Residuals")

test <- abs(rstudent(third.lm) => 3)

test[test == 1]

