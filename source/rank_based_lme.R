# |------------------------------------------------------------------------------------------------------------|
# | Project: Example of a Rank-Based Mixed Effect Model with 2 covariates                                              |
# | Script:  Rank-Based Mixed Effect Model                                                                                |
# | Depends: afex, lsmeans, multcomp                                                                           |
# | Author:  Davit Sargsyan                                                                                    | 
# | Created: 01/11/2018                                                                                        |
# | Reference: https://journal.r-project.org/archive/2013/RJ-2013-027/RJ-2013-027.pdf 
# |------------------------------------------------------------------------------------------------------------|
# Header----
require(data.table)
require(ggplot2)
require(rlme)
?rlme

# Covariates----
dt1 <- data.table(id = rep(1:3,
                           each = 3*12),
                  grp = rep(rep(1:3,
                                each = 4),
                            3*3),
                  time = rep(rep(1:4, 
                                 3), 
                             3*3))
table(dt1)

# Response----
dt1$y <- 3*dt1$grp +
  2*dt1$time + 
  rnorm(nrow(dt1),
        sd = 3)

# Add 3 outliers----
ndx <- sample(x = nrow(dt1),
              size = 3)
dt1$y[ndx] <- dt1$y[ndx] + rnorm(3,
                                 sd = 50)

# Plot the points----
ggplot(dt1,
       aes(x = time,
           y = y,
           group = factor(grp),
           fill = factor(id))) +
  geom_point(size = 2,
             shape = 21,
             position = position_dodge(0.3))

# Rank-based estimates for mixed-effects model----
m1 <- rlme(y ~ grp + time + (1 | id),
           data = dt1,
           method = "geer")
summary(m1)

# Compare to lmer----
m2 <- lmerTest::lmer(y ~ grp + time + (1 | id),
                     data = dt1)
summary(m2)