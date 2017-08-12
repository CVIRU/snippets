# Code: 3D Rotating Boxes and Crosses 
# Authors: Davit Sargsyan
# Created: 10/28/2016
#**********************************************************
require(data.table)
require(rgl)

# Data
# Number of boxes
n <- 5
d1 <- data.table(v1.est = rnorm(n),
                 v2.est = rnorm(n),
                 v3.est = rnorm(n))
d1[, c("v1.lb",
       "v1.ub",
       "v2.lb",
       "v2.ub",
       "v3.lb",
       "v3.ub") := list(v1.est - abs(rnorm(n)),
                        v1.est + abs(rnorm(n)),
                        v2.est - abs(rnorm(n)),
                        v2.est + abs(rnorm(n)),
                        v3.est - abs(rnorm(n)),
                        v3.est + abs(rnorm(n)))]
rownames(d1) <- LETTERS[1:nrow(d1)]
attach(d1)

boxes3d <- function(v1.est, v1.lb, v1.ub,
                    v2.est, v2.lb, v2.ub,
                    v3.est, v3.lb, v3.ub,
                    dim.names = c("V1", "V2", "V3"),
                    point.names = NA,
                    ...) {
  # Create basic template for 3d cubes
  a <- cube3d(trans = identityMatrix(),
              col = "blue",
              alpha = 0.2, 
              add = T)
  colnames(a$vb) <- LETTERS[1:8]
  rownames(a$vb) <- c("v1", 
                      "v2",
                      "v3",
                      "none")
  # Open new RGL device
  open3d()
  
  # Additional arguments passed through '...'
  other.args <- list(...)
  if(is.null(other.args$col)) col <- 1:length(v1.est) else col <- other.args$col
  if(is.null(other.args$size)) size <- 10 else size <- other.args$size
  if(is.null(other.args$lwd)) lwd <- rep(3, length(v1.est)) else lwd <- other.args$lwd
  
  # Plot estimates (points)
  plot3d(x = v1.est,
         y = v2.est,
         z = v3.est,
         type = "p",
         xlab = "", 
         ylab = "",
         zlab = "",
         col = col,
         size = size)
  
  # Modify 3d box templates and plot, one at at a time
  foo <- function(a, lb, ub, j) {
    
    a$material$col <- col[j]
    
    a$vb[1, c(1, 3, 5, 7)] <- lb[1, j]
    a$vb[1, c(2, 4, 6, 8)] <- ub[1, j]
    
    a$vb[2, c(1, 2, 5, 6)] <- lb[2, j]
    a$vb[2, c(3, 4, 7, 8)] <- ub[2, j]
    
    a$vb[3, c(1, 2, 3, 4)] <- lb[3, j]
    a$vb[3, c(5, 6, 7, 8)] <- ub[3, j]
    
    shade3d(a)
  }
  
  foo.v <- Vectorize(foo, "j")
  
  foo.v(a = a, 
        lb = t(data.table(v1.lb,
                          v2.lb,
                          v3.lb)), 
        ub = t(data.table(v1.ub,
                          v2.ub,
                          v3.ub)),
        j = col)
  
  
  title3d(xlab = dim.names[1],
          ylab = dim.names[2],
          zlab = dim.names[3])
  
  if (!is.na(point.names)) {
    texts3d(x = v1.est,
            y = v2.est,
            z = v3.est,
            text = point.names)
  }

  for (i in 1:length(v1.est)) {
    segments3d(x = c(v1.lb[i], v1.ub[i]),
               y = rep(v2.est[i], 2),
               z = rep(v3.est[i], 2),
               lw = lwd[i],
               col = col[i])
    segments3d(x = rep(v1.est[i], 2),
               y = c(v2.lb[i], v2.ub[i]),
               z = rep(v3.est[i], 2),
               lw = lwd[i],
               col = col[i])
    segments3d(x = rep(v1.est[i], 2),
               y = rep(v2.est[i], 2),
               z = c(v3.lb[i], v3.ub[i]),
               lw = lwd[i],
               col = col[i])
  }
  
  aspect3d(1,1,1)
}

boxes3d(v1.est, v1.lb, v1.ub,
        v2.est, v2.lb, v2.ub,
        v3.est, v3.lb, v3.ub,
        point.names = rownames(d1))


play3d(spin3d(rpm = 6,
              axis = c(0.6, 0.6, 0.6)))

# Install ImageMagick
# NOTE: version ImageMagick-7.0.3-4.x86_64.rpm (and possibly some earlier versions) do not install convert.exe by default;
# make sure to check the option box during installation
# Also missing: 'C:\Program Files\ImageMagick-7.0.3-Q16\modules\coders\IM_MOD_RL_PNG_.dll'
# reccomengation: check ALL boxes and install full verison
# CRITICAL NOTE: make sure '..._PNG_...' is capt, otherwise rename the file to make it in caps!
# Bugfix Source:
# http://stackoverflow.com/questions/20476731/animation-package-cannot-find-imagemagick-with-convert-convert/28725529#28725529
imconvertstring <- "\"C:\\Program Files\\ImageMagick-7.0.3-Q16\\convert.exe\" -delay 1x%d %s*.png %s.%s"
movie3d(f = spin3d(rpm = 6),
        duration = 10,
        convert = imconvertstring,
        dir = paste(getwd(),
                    "tmp",
                    sep = "/"))