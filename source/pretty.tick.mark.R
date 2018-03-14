# Project:      CD40 x MAdCAM-1 Duobody
# Sponsor:      Michael Scully
# Author:       Davit Sargsyan
# Created:      05/22/2013 
# Description:  Function: tick mark label as a character string
# Arguments:    x       - a number or a vector 
#               decDigits    - response means
#               sFormat   - standard errors of response means
#               maxChar   - grouping variable
#
# Requires:     
# Modified:     09/23/2014    Modified from an EZ-R function
# *****************************************************************************
pretty.tick.mark <- function(x, decDigits = 1, sFormat = "e", maxChar = 5) {
  out <- sapply(x, function(a) {
    # If the number is greater than 0, round it first
    if (!is.na(a) & is.numeric(a)) {
      if (abs(a) > 0) {
        a <- round(a, 2)
      } 
    }
    # X-axis tick mark labels
    xMark <- as.character(a)
    # If there are more than 5 digits in at least one of X values,
    # format X values to print nicely on the graph
    if (any(nchar(a) > maxChar)) {
      xMark <- formatC(x = a, digits = decDigits, format = sFormat)
    }
    return(xMark)
  })
  return(out)
}