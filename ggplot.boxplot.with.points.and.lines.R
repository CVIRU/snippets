# Snippets: ggplot boxplot with points and lines
# Author: Davit Sargsyan
# Created: 10/17/2016
################################################
require(data.table)
require(ggplot2)

# Dummy data
n.animal <- 7
n.trt <- 5
n.read <- 2

d1 <- data.table(read = factor(paste("Reading",
                                     rep(1:n.read, 
                                         each = n.animal*n.trt),
                                     sep = "")),
                 trt = factor(rep(rep(LETTERS[1:n.trt],
                                      each = n.animal),
                                  n.read)),
                 id = factor(rep(1:n.animal, n.read*n.trt)),
                 readout = rnorm(n.animal*n.trt*n.read))

# Plot
ggplot(data = d1) +
  scale_x_discrete("Treatment") + 
  scale_y_continuous("Readout") + 
  ggtitle("Title") +
  facet_wrap(~ read,
             ncol = 1) +
  geom_boxplot(aes(x = trt,
                   y = readout,
                   outlier.shape = NA)) +
  geom_point(aes(x = trt,
                 y = readout,
                 group = id,
                 colour = id),
             size = 3,
             alpha = 0.6,
             position = position_dodge(0.3)) + 
  geom_line(aes(x = trt,
                y = readout,
                group = id,
                colour = id),
            size = 2,
            alpha = 0.6,
            position = position_dodge(0.3)) + 
  guides(colour = guide_legend(title = "ID",
                               title.position="top",
                               nrow = 1)) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))