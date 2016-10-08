# Source:
# http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/

require(data.table)
require(ggplot2)

ggplot(trt,
       aes(x = as.numeric(rev(Concentration)),
           y = Mean,
           fill = Treatment)) +
  facet_wrap(~ Time,
             ncol = 1) +
  geom_bar(position = position_dodge(),
           stat="identity") +
  geom_errorbar(aes(ymax = Mean + SD,
                    ymin = Mean - SD),
                width =.2,
                position = position_dodge(.9)) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("Concentration (nM)",
                   breaks = c(0:8 - 0.2, 0:8 + 0.2),
                   labels = c(dilut.nM[-1], dilut.nM[-1])) +
  scale_y_continuous("%DMSO") +
  ggtitle("%DMSO Average +/- SD by Time, Treatment and Concentration") +
  guides(fill = guide_legend(title = "Treatment",
                               title.position = "top",
                               nrow = 1))