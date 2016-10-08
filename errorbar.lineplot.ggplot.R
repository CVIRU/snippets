# Code: Lineplot with Error Bars
# Created: 09/16/2016
# Author: Davit Sargsyan

require(data.table)
require(ggplot2)

# Data
dt <- data.table(x = rep(1:5, each = 20),
                 y = rnorm(100),
                 z = rep(c("A", "B"), 10))

# Example1: Single factor
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

# Example 2: two factors
# By x and z
dt[, mu := mean(y),
   by = list(x, z)]
dt[, std := sd(y),
   by = list(x, z)]
d1 <- unique(subset(dt, select = -c(2)))

ggplot(d1,
       aes(x = x,
           y = mu,
           colour = z,
           group = z)) +
  facet_wrap(~ z,
             ncol = 1) +
  geom_line(position = position_dodge(0.3),
            size = 1) +
  geom_point(position = position_dodge(0.3),
             size = 3) +
  geom_errorbar(aes(ymax = mu - std,
                    ymin = mu + std),
                width =.4,
                size = 1,
                position = position_dodge(0.3)) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("X") +
  scale_y_continuous("Mean(Y)") +
  ggtitle("Two-factor Errorbar Plot") +
  guides(fill = guide_legend(title = "Treatment",
                             title.position = "top",
                             nrow = 1))