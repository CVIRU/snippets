# Comments:   Plot parameters with 95% C.I. in 3D
# 
# Arguments:  obj         Robust ICF object
#             size  Size of points
#             col         Vector of colors (can be of length 1)
#
# Return:     3D plot
# 
# Requires:   rgl
# 
# Date          Developer     Action
# *****************************************************************************
# 01/14/2016    Davit         Created for ricf_1.4
# *****************************************************************************
rect3d.robicf <- function(obj, 
                          size = 10, 
                          col = NULL) {
  require(rgl)
  
  t1 <- print(res, export = TRUE)
  est <- data.frame(t1$est)[rownames(t1$est)%in%c("Bottom", "Top", "Hill Slope"),]
  lb <- data.frame(t1$lb95)[rownames(t1$est)%in%c("Bottom", "Top", "Hill Slope"),]
  ub <- data.frame(t1$ub95)[rownames(t1$est)%in%c("Bottom", "Top", "Hill Slope"),]
  
  a <- cube3d(trans = identityMatrix(),
              col = "blue",
              alpha = 0.2, 
              add = T)
  colnames(a$vb) <- LETTERS[1:8]
  rownames(a$vb) <- c("Bottom", 
                      "Top",
                      "Slope",
                      "None")
  
  if (is.null(col)) {col <- 1:ncol(est)}
  
  open3d()
  
  plot3d(x = est[1, ],
         y = est[2, ],
         z = est[3, ],
         type = "p",
         xlab = "", 
         ylab = "",
         zlab = "",
         col = col,
         size = size)
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
  
  foo.v(a, lb, ub, 1:ncol(est))
  
  title3d(xlab = "Bottom",
          ylab = "Top",
          zlab = "Slope")
  
  aspect3d(1,1,1)
}