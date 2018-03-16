# Project:      CD40 x MAdCAM-1 Duobody
# Sponsor:      Michael Scully
# Author:       Davit Sargsyan
# Created:      10/08/2014
# Description:  Function: compute means, se and medians for each group
# Arguments:    x - data
#               group - grouping variable
#
# Requires:     
# Modified:     
##################################################################################################
summ.stats <- function(x, group) {
  out <- aggregate(x = x,
                   by = group, 
                   FUN = "mean", 
                   na.rm = T)
  out$se <- aggregate(x = x, 
                      by = group, 
                      FUN = function(a) {
                        return(sd(a, na.rm = TRUE)/sqrt(sum(!is.na(a))))
                      }) [, (length(group) + 1)]
  out$med <- aggregate(x = x, 
                       by = group, 
                       FUN = "median", 
                       na.rm = T)[, (length(group) + 1)]
  names(out) <- c(names(group),
                  "Mean",
                  "Std. Error",
                  "Median")
  return(out)
}