# Project:      CD40 x MAdCAM-1 Duobody
# Experiment:   MCD4-JJPRD-19 
# Sponsor:      Michael Scully
# Author:       Davit Sargsyan
# Created:      10/15/2014  
# Description:  Display p-values nicely: 3rd decimal place or '<0.001'
# Modified:  
##################################################################################################
nice.pval <- function(x, digits = 3) {
  pVal <- x
  
  pval.small <- (x < 0.001)
  if (sum(pval.small) > 0) {
    pVal[pval.small] <- "< 0.001"
  }
  
  pval.large <- (x >= 0.001)
  if (sum(pval.large) > 0) {
    pVal[pval.large] <- round(x[pval.large], 
                              digits = digits)
  }
  
  return(pVal)
}