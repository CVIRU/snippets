# Project: Matching 
# Author: Davit Sargsyan
# Created: 10/08/2016
# Description: R code for exact and propensity score matching

require(data.table)

# Generate data using the follwing script:
# match.data.simulation.R
# Or load a previously created data set
load("match.data.RData")
summary(match.data)

# Make a copy of the dataset and use it
d1 <- match.data
rm(match.data)
gc()

# Define cases:
# 1. Patients with main diagnosis between 2000 and 2010
main.adm <- subset(d1, DiagMain == 1 & Date > "1994-12-31" & Date < "2011-01-01")
summary(main.adm)
