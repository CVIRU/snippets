# |------------------------------------------------------------------------------------------------------------|
# | Project: Example of a mixed effect vs. stratified analysis of matched data                                 |
# | Script:                                                                                                    |
# | Depends: data.table, ggplot2, lmerTest, survival                                                           |
# | Author:  Davit Sargsyan                                                                                    | 
# | Created: 03/16/2018                                                                                        |
# |------------------------------------------------------------------------------------------------------------|
# Header----
require(data.table)
require(ggplot2)
require(lmerTest)
require(survival)

# Number of pairs----
n.pair <- 100

# Simulate data with continuous response----
dt1 <- data.table(id = 1:(2*n.pair),
                  pair = rep(1:n.pair,
                             each = 2),
                  grp = rep(1:2,
                            rep = n.pair),
                  x1 = rep(sample(x = c(0, 1),
                                  size = n.pair,
                                  replace = TRUE),
                           each = 2),
                  x2 = rep(sample(x = c(0, 1),
                                  size = n.pair,
                                  replace = TRUE),
                           each = 2))
dt1$y <- 5*dt1$grp + 3*dt1$x1 + 0*dt1$x2 + rnorm(n = 2*n.pair,
                                                 mean = 0,
                                                 sd = 3)
dt1

# Mixed effects model with pair as random----
m1 <- lmerTest::lmer(y ~ grp + x1 + x2 + (1 | pair),
                     data = dt1)
summary(m1)

ggplot(dt1,
       aes(x = factor(grp),
           y = y,
           fill = factor(x1))) +
  geom_boxplot()

# Simulate data wih binary response----
dt1 <- data.table(id = 1:(2*n.pair),
                  pair = rep(1:n.pair,
                             each = 2),
                  grp = rep(1:2, 
                            rep = n.pair),
                  ndrugs = sample(0:6, 
                                  size = 2*n.pair,
                                  replace = TRUE))

dt1

nu <- (0.5*dt1$grp + 0.1*dt1$ndrugs + rnorm(n = 2*n.pair,
                                        mean = 0,
                                        sd = 1))

dt1$prob <- exp(nu)/(1 + exp(nu))
dt1$y <- dt1$prob > 0.8
summary(dt1)

# Stratified logistic regression----
m1 <- clogit(y ~ grp + ndrugs + strata(pair),
             data = dt1)
m1

# sink()