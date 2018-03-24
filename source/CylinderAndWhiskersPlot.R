# Author: Davit Sargsyan
# Created: 08/02/2013

require(rgl)

#DATA----
nX1 <- 3
nX2 <- 3
xx1 <- (1:nX1)
xx2 <- (1:nX2)
nRpl <- 10
x1 <- as.factor(rep(LETTERS[xx1], nX2 * nRpl))
x2 <- as.factor(rep(letters[xx2], each = nX1 * nRpl))
x1
x2
y <- rnorm(nX1 * nX2 * nRpl)
y

# Plot boxplots
bp <- boxplot(y ~ x1 + x2, col = rep((xx1 + 2), nX2))
bp
bp$stats

# Top and bottom
bot <- matrix(bp$stats[1, ], nrow = nX1, ncol = nX2)
top <- matrix(bp$stats[5, ], nrow = nX1, ncol = nX2)

# Radius
r <- 0.2

# Create a system of coordinates
plot3d(xx1, xx2, bot, col = "blue", size = 10, type = "n")

# Coordinates of the cylinders (Q1 and Q3)
hCyl <- array(data = NA, dim = c(nX1, nX2, 2))
# rng <- top - bot
# hCyl[,,1] <- bot + 0.25*rng
# hCyl[,,2] <- top - 0.25*rng
hCyl[,,1] <- matrix(bp$stats[2, ], nrow = nX1, ncol = nX2)
hCyl[,,2] <- matrix(bp$stats[4, ], nrow = nX1, ncol = nX2)
hCyl

# Medians (Q2)
med <- matrix(bp$stats[3, ], nrow = nX1, ncol = nX2)

# Plot cylinders
for (i in 1:nX1) {
  for (j in 1:nX2) {
    plot3d(xx1[i], xx2[j], c(bot[i, j], top[i, j]), col = "blue", 
           type = "p", add = T, size = 10)
    plot3d(xx1[i], xx2[j], c(bot[i, j], top[i, j]), col = "blue", 
           type = "l", add = T)
    center <- cbind(xx1[i], xx2[j], seq(hCyl[i, j, 1], hCyl[i, j, 2], len = 2))
    cyl <- cylinder3d(center = center, 
                      radius = r, twist = 0, e1 = NULL, e2 = NULL, 
                      e3 = NULL, sides = 30, section = NULL, closed = -2,
                      debug = FALSE, keepVars = FALSE) 
    # Median
    n <- 300
    theta <- seq(0, 2 * pi, len=n)
    x <- cos(theta) * r + xx1[i]
    y <- sin(theta) * r + xx2[j]
    z <- rep(med[i, j], n)
    lines3d(x,y,z)
    
    #shade3d(addNormals(subdivision3d(cyl, depth = 2)), col = i + 2)
    # Correction by Duncan Murdoch to get rid of teh rounding at the ends (08/07/2013)
    shade3d(cyl, col = i + 2)
  }
}

# Outliers
lX1 <- rep(xx1, nX2)
lX2 <- rep(xx2, each = nX1)
ndx <- bp$group
plot3d(lX1[ndx], lX2[ndx], bp$out, add = T, col = "red", size = 10, type = "p")

aspect3d(nX1, nX2, min(nX1, nX2))

#axis3d(labels = c("X", "Y", "Z"))
#play3d(spin3d(axis=c(0,0,1), rpm=1), duration=60)