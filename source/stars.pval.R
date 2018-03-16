# Project:      
# Experiment:    
# Sponsor:      
# Author:       Davit Sargsyan
# Created:      03/09/2015
# Description:  Display p-value stars: ** if < 0.01; * if < 0.05
# Arguments:    x - vector of p-values
# Modified:  
##################################################################################################
stars.pval <- function(x) {
  Star <- rep("", length(x))
  
  one.star <- (x <= 0.05)
  if (sum(one.star) > 0) {
    Star[one.star] <- "_*_"
  }
  
  two.stars <- (x <= 0.01)
  if (sum(two.stars) > 0) {
    Star[two.stars] <- "_**_"
  }
  
  return(Star)
}