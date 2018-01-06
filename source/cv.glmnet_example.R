require(data.table)

x = matrix(rnorm(10000),
           nrow = 100)

x1 <- matrix(matrix(sample(c(0, 1),
                           size = 10000,
                           replace = TRUE)),
             nrow = 100)
y = sample(c(0, 1),
           size = 100,
           replace = TRUE)

boot::cv.glm()

glm(y ~ x1,
    family = "binomial")

require(glmnet)
m1 <- glmnet::cv.glmnet(x = x1,
                  y = y,
                  nfolds = 10,
                  family = "binomial")
plot(m1)

m1$lambda.min

m2 <- glmnet(x, y, 
             alpha = 1,
             lambda = m1$lambda.min)
m2$beta
predict(m2,
        newx = x)
