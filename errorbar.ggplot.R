# Code: Lineplot with Error Bars
# Created: 09/16/2016
# Author: Davit Sargsyan

# Data
require(data.table)
require(ggplot2)

dt <- data.table(x = rep(1:5, each = 20),
                 y = rnorm(100))

# Compute means and standard deviations
dt[, mu := mean(y),
   by = x]
dt[, std := sd(y),
   by = x]
d1 <- unique(subset(dt, select = -2))

# Plot means and error bars
ggplot(d1, 
       aes(x = x, 
           y = mu)) + 
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mu - std, 
                    ymax = mu + std),
                colour = "black", 
                width = .1) 