# Code: Lineplot with Error Bars
# Created: 11/23/2016
# Author: Davit Sargsyan
# Source:
# https://assessingpsyche.wordpress.com/2014/06/04/using-the-truncated-normal-distribution/
#*****************************************************************************************
# Data simulation is based on reported values from:
# "Efficacy and safety of alirocumab in reducing lipids and cardiovascular events", 
# Robinson et al, The New England Journal of Medicine, April 2015
# https://www.ncbi.nlm.nih.gov/pubmed/25773378

# Data----
mu <- 48.3
sem <- 0.9
N <- 1530
std <- sem*sqrt(N)
x <- seq(0, 100, 0.1)
y <- dnorm(x = x, 
           mean = mu, 
           sd = std)

# Function to calculate truncated distribution mean
MeanNormalTruncated <- function(mu = 0,
                                sigma = 1,
                                a = -Inf,
                                b = Inf){
  mu + sigma*(dnorm((a - mu)/sigma) - dnorm((b - mu)/sigma))/
    (pnorm((b - mu)/sigma) - pnorm((a - mu)/sigma))
}

#*****************************************************************************************
# Calculate and plot expected values for given threshods----
# th <- 15
th <- 25

mu1 <- MeanNormalTruncated(mu = mu,
                           sigma = std,
                           a = 0,
                           b = th)
mu1

mu2 <- MeanNormalTruncated(mu = mu,
                           sigma = std,
                           a = th,
                           b = 100)
mu2

plot(y ~ x,
     type = "l",
     ylim = c(0, 0.015),
     xlab = "LDL",
     ylab = "Probability",
     main = paste(" Simulation of Alirocumab Effect on LDL",
                  "\n Mean = 48.3, SEM = 0.9, N = 1530, Threshold =",
                  th))
polygon(x = c(0, 
              seq(0, th, 0.1),
              th),
        y = c(0, 
              dnorm(x = seq(0, th, 0.1),
                    mean = mu,
                    sd = std),
              0),
        angle = 45,
        density = 10)
polygon(x = c(th, 
              seq(th, 100, 0.1),
              100),
        y = c(0, 
              dnorm(x = seq(th, 100, 0.1),
                    mean = mu,
                    sd = std),
              0),
        angle = -45,
        density = 10)
abline(v = c(mu1, mu2),
       lty = 2)
text(x = c(mu1, mu2),
     y = c(0.008, 0.012),
     labels = round(c(mu1, mu2)))