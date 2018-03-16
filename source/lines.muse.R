# Project:      CD40 x MAdCAM-1 Duobody
# Sponsor:      Michael Scully
# Author:       Davit Sargsyan
# Created:      08/27/2012 
# Description:  Function: lineplot of means with error bars 
# Arguments:    x       - X-axis data
#               mu.y    - response means
#               se.y    - standard errors of response means
#               group   - grouping variable
#               ttl     - plot title
#               lgd     - boolean: should legend be displayed?
#               ttl.lgd - legend title
#               wd      - plot width
#               ht      - plot height
#               err.bar - boolean: should error bars be plotted?
#               x.lab   - X-axis label
#               y.lab   - Y-axis label
#               x.lim   - X-axis limit
#               x.rot   - X-axis label rotation
#               export  - boolean: should the graph be saved as a .PNG file?
#               path    - full name of the .PNG file to be saved
#
# Requires:     
# Modified:     09/23/2014    Modified from an EZ-R function
##################################################################################################
lines.muse <- function(x, 
                       mu.y, 
                       se.y  = NULL, 
                       group = NULL,
                       ttl,
                       lgd = TRUE, 
                       ttl.lgd = "Legend",
                       wd = 480, 
                       ht = 480, 
                       err.bar = TRUE,
                       x.lab = "X", 
                       y.lab = "Y", 
                       x.lim = NULL, 
                       x.rot = 45, 
                       export = FALSE, 
                       path = NULL,
                       color = NA) {
 
  # Save default parameters
  def.par <- par(no.readonly = TRUE)

  # Start PNG if plots must be exported
  if (export) {
    gname <- paste(path, "\\lines.png", sep = "")
    png(filename = gname, width = wd, height = ht)
  }
  
  # Y limit    
  if (!is.null(se.y)) {
    if (err.bar & !(all(is.na(se.y)))) {
      y.lim <- c(min(mu.y - se.y , na.rm = T), max(mu.y + se.y , na.rm = T))
    } else {
      y.lim <- range(mu.y,na.rm=T)
    }
  } else {
    y.lim <- range(mu.y,na.rm=T)
  }
      
  # Create X axis matrix
  # Spread point by 1/100 of the X range between treatmen means
  deltaX <- diff(range(x))/(20 * length(x))
    
  # Create a matrix of offsets      
  offsetX <- matrix(c(0:(nrow(mu.y) - 1)) * deltaX, nrow = nrow(mu.y), 
                    ncol = length(x), byrow = FALSE)
  
  # Create a matrix of X-axis values offseted by deltaX      
  xx <- matrix(x, nrow = nrow(mu.y), ncol = length(x), byrow = TRUE) + offsetX
  
  # Overwrite X axis limit
  x.lim <- c(min(xx, na.rm = T), max(xx, na.rm = T))
  
  # Legend (opt): if either Group or ID is not a NULL
  # Text zoom: none (default)
  zoomText <- 1
  if(lgd & (!is.null(group))) {
    # Scale factor
    x.scale <- diff(x.lim)
    y.scale <- diff(y.lim)
    
    # Set layout
    layout(mat = matrix(c(0, 1, 0, 0, 2, 0), 3, 2, byrow = FALSE), 
           width = c(0.5, 0.5)*x.scale, 
           heights = c(0.1, 0.8, 0.1)*y.scale)
    
    # Text zoom; time and a half
    #zoomText <- 1.1
  }
  
  # Color scheme
  if (is.na(color[1])) color <- rainbow(nrow(mu.y))

######################################################################
  # Plot means
  matplot(x = t(xx), y = t(mu.y), type = "o", pch = 16, lty = 1, col = color,
          main = ttl, xlab = x.lab, cex.lab = zoomText, 
          cex.main = zoomText, cex.axis = zoomText, 
          ylab = y.lab, ylim = y.lim, 
          xlim = x.lim, 
          xaxt = "n")
  
  # X-axis tick mark labels
  xMarks <- pretty.tick.mark(x)
  
  # X-axis tick marks
  axis(side = 1, at = x, labels = FALSE)
  # X-axis labels
  text(x = x, y = y.lim[1] - 0.05 * (y.lim[2] - y.lim[1]), srt = x.rot,
       pos = 1, labels = xMarks, xpd = T, cex = zoomText)
  
  # Error Bars (opt)
  if (err.bar & !is.null(se.y )) {
      for (i in 1:nrow(se.y )) {
        # NOTE: ERROR!
        for (j in 1:ncol(mu.y)) {
          errbar(xx[i, j],
                 mu.y[i, j],
                 mu.y[i, j] + se.y [i, j],
                 mu.y[i, j] - se.y [i, j],
                 col = color[i],
                 add = TRUE)
        }
      }
  }

  # Print Legend
  if (lgd & !is.null(group)) {
    # Eliminate margins in the second sector
    #par(mar = c(0, 0, 0, 0))
    
    # Empty plot
    frame()
    # Legend
    legend("left", legend = unique(group), xpd = TRUE, 
           bty = "n", col = color, lty = 1, 
           pch = 16, title = ttl.lgd, cex = zoomText)
  }

  # Reset parameters
  par(def.par)

  # Close PNG
  if (export) {
    #coreAddGraph(gname)
    graphics.off()
  }

  #Return nothing
  return()
}