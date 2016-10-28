# Snippets: cox regression and plots
# Author: Davit Sargsyan
# Created: 10/17/2016
################################################
require(data.table)
require(survival)

n <- 100

d1 <- data.table(event = sample(x = c(FALSE, TRUE),
                                size = n,
                                replace = TRUE,
                                prob = c(0.8, 0.2)),
                 days2event = sample(x = 1:1000,
                                     size = n,
                                     replace = TRUE),
                 age = sample(x = 60:90,
                              size = n,
                              replace = TRUE),
                 sex = factor(sample(x = c("M", "F"),
                                     size = n,
                                     replace = TRUE),
                              levels = c("M", "F")))
summary(d1)

# Sensor
d1$days2event[d1$event == 0] <- 1001

m1 <- coxph(Surv(days2event, event) ~ age + sex, 
            data = d1)
m1
summary(m1)

predict(m1,
        newdata = list(age = 75,
                       sex = "M"),
        type = "expected")

# Plot survival curves
plot(survfit(m1),
     mark.time = FALSE)

# 1 year survival
d1$event.1y <- 0
d1$event.1y[d1$event == 1 & d1$days2event < 366] <- 1
d1$event.1y <- factor(d1$event.1y)
summary(d1)

m2 <- glm(event.1y ~ age + sex,
          family = "binomial",
          data = d1)
m2
summary(m2)

predict(m2,
        newdata = list(age = 75,
                       sex = "M"))
