# Code: Barplot with Error Bars   
# Authors: Davit Sargsyan
# Created:  09/16/2016
# Source: http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/
#**********************************************************
require(data.table)
require(ggplot2)

# Data----
dt1 <- data.table(x = rep(1:5, each = 20),
                 y = abs(rnorm(100)),
                 z = rep(c("A", "B"), 10))

# Summary by x and z
dt1[, mu := mean(y),
   by = list(x, z)]
dt1[, std := sd(y),
   by = list(x, z)]
d1 <- unique(subset(dt1, select = -c(2)))

# Plot----
ggplot(d1,
       aes(x = x,
           y = mu,
           fill = z)) +
  facet_wrap(~ z,
             nrow = 1) +
  geom_errorbar(aes(ymax = mu + std,
                    ymin = mu),
                width =.2,
                position = position_dodge(.9)) +
  geom_point(position = position_dodge(0.9),
             size = 3) +
  geom_bar(position = position_dodge(),
           stat="identity") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("X Variable",
                   breaks = 1:5,
                   labels = paste("Group", 
                                  unique(d1$x))) +
  scale_y_continuous("Group Averages") +
  ggtitle("Title") +
  guides(fill = guide_legend(title = "Subgroups",
                               title.position = "top",
                               nrow = 1))