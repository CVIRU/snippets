# Project:      CD40 x MAdCAM-1 Duobody
# Sponsor:      Michael Scully
# Author:       Davit Sargsyan
# Created:      10/23/2014 
# Description:  Function: lineplot of means with error bars 
# Arguments:    x       - X-axis data
#               mu.y    - response means
#               se.y    - standard errors of response means
#               group   - grouping variable
#               color   - color scheme
#
# Requires:     Hmisc
# Modified:     
##################################################################################################
bars.muse <- function(x, 
                      mu.y, 
                      se.y, 
                      color,
                      group = NULL,
                      main = "AUCs with S.E. Bars",
                      xlab = "Treatment",
                      ylab = "AUC",
                      y.lim = NULL,
                      starz = NULL,
                      starz.xpos = NULL,
                      starz.ypos = NULL,
                      legend = TRUE) {
  require(Hmisc)
  
  # Save default parameters
  def.par <- par(no.readonly = TRUE)

  bar <- mu.y + se.y
  bar[mu.y < 0] <- mu.y[mu.y < 0] - se.y[mu.y < 0]
  
  # Get X-axis values
  #x.axis <- barplot(mu.y)
  x.axis <- 1:length(mu.y)
  
  if (legend) {
    # Legend Scale Factor
    x.scale <- diff(range(x.axis))
    y.scale <- diff(range(bar))
    
    # Set layout
    layout(mat = matrix(c(0, 1, 0, 0, 2, 0), 3, 2, byrow = FALSE), 
           width = c(0.5, 0.5)*x.scale, 
           heights = c(0.1, 0.8, 0.1)*y.scale)
  }

  if (is.null(y.lim)) {
    y.lim <- range(bar)
    if(y.lim[1] > 0) {y.lim[1] <- 0}
  }
  
  tmp <- plot(bar ~ seq(from = 0,
                        to = length(bar) + 2.5,
                        length.out = length(bar)), 
              type = "n",
              xaxt = "n",
              main = main,
              xlab = xlab,
              ylab = ylab,
              ylim = y.lim)
  
  x.vals <- barplot(mu.y, 
                    col = color,
                    ylim = y.lim,
                    xpd = FALSE,
                    add = TRUE)
  
  tmp <- errbar(x = x.vals,
                y = mu.y,
                yplus = bar,
                yminus = mu.y,
                add = TRUE)
  
  tmp <- axis(side = 1, 
              at = x.vals, 
              labels = LETTERS[1:length(bar)])
  
  # Stars
  if(!is.null(starz)) {
    tmp <- text(x = x.vals[starz.xpos],
                y = starz.ypos,
                labels = starz)
  }
  
  if (legend) {
    # Empty plot
    frame()
    # Legend
    if (is.null(group)) {
      leg <- paste(LETTERS[1:length(bar)],
                   unique(x),
                   sep = ": ")
    } else {
      leg <- paste(LETTERS[1:length(bar)],
                   group,
                   sep = ": ")
    }
    tmp <- legend("left", 
                  legend = leg,
                  xpd = TRUE, 
                  bty = "n", 
                  col = color, 
                  lty = 1, 
                  pch = 16, 
                  title = "Legend",
                  cex = 1.2)
  }
  
  # Reset parameters
  par(def.par)

  #Return nothing
  return(NULL)
}