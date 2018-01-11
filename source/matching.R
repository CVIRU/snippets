# Project: Matching 
# Author: Davit Sargsyan
# Created: 10/08/2016
# Description: R code for exact and propensity score matching

require(data.table)
require(lme4)

# Generate data using the follwing script:
# match.data.simulation.R
# Or load a previously created data set
load("match.data.RData")
summary(match.data)

# Make a copy of the dataset and use it
d1 <- match.data
rm(match.data)
gc()

# Step1: compute propensity scores---- 
# i.e. propensity of each subject for having Condition B
# NOTE: include Condition A as a covariate
d1$ID <- factor(d1$ID, levels = unique(d1$ID))
m1 <- glmer(CondB ~ CondA + Diag1 + Diag2 + Diag3 + Diag4 + Diag5 + (1|ID),
            data = d1,
            family = "binomial")
summary(m1)

# Predicted odd ratios for Condition A
pp <- predict(m1,
              type = "response")
summary(pp)
hist(pp, 20)

# Save propensity scores (negative log of probabilities)
d1$pp <- (-log(pp))
hist(d1$pp, 20)

#######################################################
# Step2: define cases and controls
# 1. CASES: Patients with Conditon A diagnosis between 2000 and 2010
main.adm <- subset(d1, CondA == 1 & Date > "1999-12-31" & Date < "2011-01-01")
summary(main.adm)
id.case <- unique(main.adm$ID)

# All records for cases
case.all <- subset(d1, ID %in% id.case)
setkey(case.all, ID, Date)
length(unique(case.all$ID))
summary(case.all)

# 2. CONTROLS: everybody else
# All records for controls
ctrl.all <- subset(d1, !(ID %in% id.case))
setkey(ctrl.all, ID, Date)
length(unique(ctrl.all$ID))
summary(ctrl.all)

#######################################################
# Step3: calculate histories of diagnosis for cases
# 1. Date of first admission for Condition A (between 2000 and 2010)
case.all[, FirstAdm := min(Date[Date > "1999-12-31" & 
                                  Date < "2011-01-01" & 
                                  CondA == 1]),
         by = ID]

# 2. Age at first admission
case.all[, FirstAdmAge := as.numeric(floor(difftime(FirstAdm,
                                                    Birthday)/365.25)),
         by = ID]

# 3. Year of first admission
case.all[, FirstAdmYear := as.numeric(format(FirstAdm, "%Y")),
         by = ID]

# 4. History of diagnosis
hh <- case.all[ , list(hCondA = as.numeric(sum((CondA == 1) & 
                                                 Date < FirstAdm) > 0),
                       hCondB = as.numeric(sum((CondB == 1) & 
                                                 Date < FirstAdm) > 0),
                       hDiag1 = as.numeric(sum((Diag1 == 1) & 
                                                 Date < FirstAdm) > 0),
                       hDiag2 = as.numeric(sum((Diag2 == 1) & 
                                                 Date < FirstAdm) > 0),
                       hDiag3 = as.numeric(sum((Diag3 == 1) & 
                                                 Date < FirstAdm) > 0),
                       hDiag4 = as.numeric(sum((Diag4 == 1) & 
                                                 Date < FirstAdm) > 0),
                       hDiag5 = as.numeric(sum((Diag5 == 1) & 
                                                 Date < FirstAdm) > 0)),
                       by = ID]

# 5. Merge cases with history
cases <- merge(subset(case.all,
                      subset = (Date == FirstAdm),
                      select = c(1, 14, 15, 16, 3, 4, 13)),
               hh,
               by = "ID")
summary(cases)

#######################################################
# Step4: additional calculations for controls
# a. Date of first admission for any condition except Condition A (between 2000 and 2010)
ctrl.all[, FirstAdm := min(Date[Date > "1999-12-31" & 
                                  Date < "2011-01-01"]),
         by = ID]
# NOTE: some controls might not have any admisions between 2000 and 2010
# Remove them
ctrl.all <- subset(ctrl.all, 
                   subset = (as.character(FirstAdm) != "NA"))

# b. Age at each admission
ctrl.all$AdmAge <-  as.numeric(floor(difftime(ctrl.all$Date,
                                              ctrl.all$Birthday)/365.25))

# c. Year of each admission
ctrl.all$AdmYear <-  as.numeric(format(ctrl.all$Date, "%Y"))

#######################################################
# Step5:
# a. shuffle and randomly select a case and match him/her to controls
cases <- cases[sample(1:nrow(cases), 
                      nrow(cases),
                      replace = FALSE), ]
i = 4
case.i <- cases[i, ]
case.i

# b. Exact match by sex and race, 
# approximately by age and year of admission (+/- 2 years),
# and by propensity scores (+/-1)
ctrl.pool <- subset(ctrl.all,
                    Sex == case.i$Sex &
                      Race == case.i$Race &
                      (abs(AdmAge - case.i$FirstAdmAge) < 2) & 
                      (abs(AdmYear - case.i$FirstAdmYear) < 2) & 
                      abs(pp - case.i$pp) < 0.2)
case.i
ctrl.pool

# c. Select a control at random from the pool
ctrl.id <- as.character(sample(unique(ctrl.pool$ID), 1))
ctrl.id

# FINISH THIS CODE!
# 1. Find the selected control's admission closest to 
# the case's first admission for Cndition A data and 
# create a vector of history similar to Step3-4
# 2. Look for recorded outcome (Condition B) for both, 
# case and control, after first admissions
# 3. Remove selected case and control from data, save to a new table
# 4. Once all cases are matched, repeat matching for next set of controls
# or proceed to the analysis (Cox or logistic regression)

# Other things to consider:
# 1. Time from an admisson to the clock start. 
# The inverse can be used similar to propensity score