# |------------------------------------------------------------------------------------------------------------|
# | Project: Example of a Mixed Effect Model with 2-Way Contrasts                                              |
# | Script:  Mixed Effect model                                                                                |
# | Depends: afex, lsmeans, multcomp                                                                           |
# | Author:  Davit Sargsyan                                                                                    | 
# | Created: 07/18/2017                                                                                        |
# | Source:  https://stats.stackexchange.com/questions/157022/testing-contrast-in-two-way-anova-using-multcomp |
# |------------------------------------------------------------------------------------------------------------|
# Header----
library(afex)
require(lsmeans)
require(multcomp)

# Load example dataset----
data(obk.long)

# Fint a mixed effect model with two covariates and an interaction----
fit <- aov_car(value~treatment*gender + Error(id),
               data=obk.long,
               return = "aov")
fit
summary(fit)
plot(fit)

# Compute summary statistics----
ref1 <- lsmeans(fit, 
                c("treatment", 
                  "gender"))
ref1
# Forest plot of means----
plot(ref1)

# Set contrasts based on 'ref1' 
c_list <- list(`B - A, F` = c(0, -1, 1, 0, 0, 0),
               `B - A, M` = c(0, 0, 0, 0, -1, 1),
               `B - A, F + M` = c(0, -0.5, 0.5, 0, -0.5, 0.5))

# Pairwise comp[arison
c1 <- contrast(ref1, c_list)
c1 
# Forest plot of contrasts----
plot(c1)

# Bonferroni-Holm correction to control for Type I error----
s1 <- summary(c1,
              adjust = "holm")
s1

# Using glht----
s2 <- summary(as.glht(contrast(ref1, c_list)), 
              test = adjusted("free"))
s2
# Ferest plot of contrasts----
plot(s2)