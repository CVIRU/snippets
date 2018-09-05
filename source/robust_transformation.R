# |-------------------------------------------------------------|
# | Project: Robust transformation with Tukey biweights         |
# | Script:                                                     |
# | Author:  Davit Sargsyan                                     | 
# | Created: 09/05/2018                                         |
# | Source:                                                     |
# |-------------------------------------------------------------|

# Data----
require(data.table)
require(ggplot2)
require(MASS)

# Data----
dt1 <- data.table(trt = factor(rep(LETTERS[1:3], each = 10)))
dt1$y <- 5*as.numeric(dt1$trt) + 10 + rnorm(3*10)
dt1

# Contaminate
dt1$y[1] <- 50

ggplot(dt1,
       aes(y = y,
           x = trt)) +
  geom_boxplot() +
  geom_point()

# Linear model----
m1 <- lm(y ~ trt,
         data = dt1)
summary(m1)
anova(m1)

# Weights----
wgt <- psi.bisquare(m1$residuals)
wgt

# Reweighted response----
dt1$y2 <- dt1$y*wgt
ggplot(dt1,
       aes(y = y2,
           x = trt)) +
  geom_boxplot() +
  geom_point()

# Reweighted modeling----
m2 <- lm(y2 ~ trt,
         data = dt1)
summary(m2)
anova(m2)