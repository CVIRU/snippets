# Project: Matching 
# Author: Davit Sargsyan
# Created: 10/08/2016
# Description: R code for matching data simulation

require(data.table)

# Reproducibility
set.seed(100)

# Number of subjects
n <- 500
# Phenotype
dummy <- data.table(ID = 1:n,
                    Birthday = sample(seq(as.Date("1925/01/01"), 
                                          as.Date("1950/12/31"),
                                          by = "day"), 
                                      size = n,
                                      replace = TRUE),
                    Sex = factor(sample(c("F", "M"),
                                        size = n,
                                        replace = TRUE),
                                 levels = c("F", "M")),
                    Race = factor(sample(c("White", "Black", "Other"),
                                         size = n,
                                         replace = TRUE,
                                         prob = c(0.7, 0.2, 0.1)),
                                  levels = c("White", "Black", "Other")))
summary(dummy)

# Average number of records per patient
nrec <- 5

# Diagnosis
dx <- data.table(ID = sample(dummy$ID,
                             size = nrec*n,
                             replace = TRUE),
                 Date = sample(seq(as.Date("1995/01/01"), 
                                   as.Date("2011/01/01"),
                                   by = "day"), 
                               size = nrec*n,
                               replace = TRUE),
                 CondA = sample(0:1,
                                size = nrec*n,
                                replace = TRUE,
                                prob = c(0.7, 0.3)),
                 CondB = sample(0:1,
                                size = nrec*n,
                                replace = TRUE,
                                prob = c(0.9, 0.1)),
                 Diag1 = sample(0:1,
                                size = nrec*n,
                                replace = TRUE,
                                prob = c(0.8, 0.2)),
                 Diag2 = sample(0:1,
                                size = nrec*n,
                                replace = TRUE,
                                prob = c(0.9, 0.1)),
                 Diag3 = sample(0:1,
                                size = nrec*n,
                                replace = TRUE,
                                prob = c(0.5, 0.5)),
                 Diag4 = sample(0:1,
                                size = nrec*n,
                                replace = TRUE,
                                prob = c(0.3, 0.7)),
                 Diag5 = sample(0:1,
                                size = nrec*n,
                                replace = TRUE,
                                prob = c(0.6, 0.4)))
setkey(dx, ID)

# Set 70% of subjects 'case-free'
dx$CondA[dx$ID %in% sample(dummy$ID, floor(0.7*n))] <- 0

# Merge data sets
match.data <- merge(dummy, dx, by = "ID")
summary(match.data )

# Number of subjects (NOTE: some subjects might not be selected in dx)
length(unique(match.data $ID))
rm(dummy, dx, n, nrec)
gc()
save(match.data , file = "match.data.RData")