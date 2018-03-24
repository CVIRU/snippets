# Project: 3D Rotaing Plot
# Author: Davit Sargsyan
# Created:  10/24/2016
require(rgl)

#################################################
# Data----
df1 <- read.csv("C:\\Users\\ds752\\Documents\\git_local\\short.projects\\HT-HF meta 3D cube numberss.csv")
# df1 <- read.csv("HT-HF meta 3D cube numberss.csv")
df1[, 5:10] <- 100*df1[, 5:10]
df1

#################################################
# 3D plots----
size <- 10
col <- 1:nrow(df1)
lwd <- c(rep(3, nrow(df1) - 1), 6)

a <- cube3d(trans = identityMatrix(),
            col = "blue",
            alpha = 0.2, 
            add = T)
colnames(a$vb) <- LETTERS[1:8]
rownames(a$vb) <- c("Efficacy OR", 
                    "RD",
                    "Drop-out OR",
                    "None")

open3d()

# Points
plot3d(z = df1$Efficacy.OR.mean,
       y = df1$RD.mean,
       x = df1$Drop.out.OR.mean,
       type = "p",
       xlab = "", 
       ylab = "",
       zlab = "",
       col = col,
       size = size)

# # Boxes
# foo <- function(a, lb, ub, j) {
#   
#   a$material$col <- col[j]
#   
#   a$vb[1, c(1, 3, 5, 7)] <- lb[1, j]
#   a$vb[1, c(2, 4, 6, 8)] <- ub[1, j]
#   
#   a$vb[2, c(1, 2, 5, 6)] <- lb[2, j]
#   a$vb[2, c(3, 4, 7, 8)] <- ub[2, j]
#   
#   a$vb[3, c(1, 2, 3, 4)] <- lb[3, j]
#   a$vb[3, c(5, 6, 7, 8)] <- ub[3, j]
#   
#   shade3d(a)
# }
# 
# foo.v <- Vectorize(foo, "j")
#
# foo.v(a = a, 
#       lb = t(df1[, c("Drop.out.OR.lower",
#                      "RD.lower",
#                      "Efficacy.OR.lower")]), 
#       ub = t(df1[, c("Drop.out.OR.upper",
#                      "RD.upper",
#                      "Efficacy.OR.upper")]),
#       j = col)

title3d(zlab = "Efficacy OR",
        ylab = "RD (%)",
        xlab = "Drop-out OR")
texts3d(z = df1$Efficacy.OR.mean,
        y = df1$RD.mean,
        x = df1$Drop.out.OR.mean,
        text = df1$CLASS)

# Line segments
for (i in 1:6) {
  segments3d(z = c(df1$Efficacy.OR.lower[i], df1$Efficacy.OR.upper[i]),
             y = rep(df1$RD.mean[i], 2),
             x = rep(df1$Drop.out.OR.mean[i], 2),
             lw = lwd[i],
             col = col[i])
  segments3d(z = rep(df1$Efficacy.OR.mean[i], 2),
             y = c(df1$RD.lower[i], df1$RD.upper[i]),
             x = rep(df1$Drop.out.OR.mean[i], 2),
             lw = lwd[i],
             col = col[i])
  segments3d(z = rep(df1$Efficacy.OR.mean[i], 2),
             y = rep(df1$RD.mean[i], 2),
             x = c(df1$Drop.out.OR.lower[i], df1$Drop.out.OR.upper[i]),
             lw = lwd[i],
             col = col[i])
}

aspect3d(1,1,1)

snapshot3d(filename = "tmp/plot2.bmp")

# play3d(spin3d(rpm = 6))
# play3d(spin3d(rpm = 6,
#               axis = c(0.6, 0.6, 0.6)))
# # Rotate and save:
# pp <- par3d()
# # Rotate again and reset
# par3d(pp)
# save <- par3d(userMatrix = rotationMatrix(90*pi/180, 1, 0, 0))
# save

# Install ImageMagick
# NOTE: version ImageMagick-7.0.3-4.x86_64.rpm (and possibly some earlier versions) do not install convert.exe by default;
# make sure to check the option box during installation
# Bugfix Source:
# http://stackoverflow.com/questions/20476731/animation-package-cannot-find-imagemagick-with-convert-convert/28725529#28725529
imconvertstring <- "\"c:\\Program Files\\ImageMagick-7.0.3-Q16\\convert.exe\" -delay 1x%d %s*.png %s.%s"
movie3d(f = spin3d(rpm = 6),
        duration = 10,
        convert = imconvertstring,
        dir = paste(getwd(),
                    "tmp",
                    sep = "/"))

#################################################
# Projections----
attach(df1)

tiff(filename = "tmp/plot1.tiff",
     width = 7,
     height = 7,
     units = "in",
     res = 400)
# Source: 
# http://stackoverflow.com/questions/13239986/avoid-wasting-space-when-placing-multiple-aligned-plots-onto-one-page
par(mfrow = c(2, 2),     # 2x2 layout
    oma = c(5, 5, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(0,0,1,1),    # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA)            # allow content to protrude into outer margin (and beyond)

# 1. Efficacy OR vs Rate Difference (RD)
plot(1 ~ 1,
     type = "n",
     xlim = range(c(RD.lower,
                    RD.upper)),
     ylim = range(c(Efficacy.OR.lower,
                    Efficacy.OR.upper)),
     xlab = "",
     ylab = "Efficacy OR",
     xaxt = "n")
segments(x0 = RD.lower,
         x1 = RD.upper,
         y0 = Efficacy.OR.mean,
         y1 = Efficacy.OR.mean,
         col = 1:6,
         lw = c(2,2,2,2,2,3))
segments(x0 = RD.mean,
         x1 = RD.mean,
         y0 = Efficacy.OR.upper,
         y1 = Efficacy.OR.lower,
         col = 1:6,
         lw = c(2,2,2,2,2,3))
# text(CLASS,
#      x = 1.1*Drop.out.OR.mean,
#      y = 1.05*Efficacy.OR.mean)
legend("topright",
       legend = CLASS,
       lty = 1,
       col = 1:6,
       lw = c(2,2,2,2,2,3))

# 2. Efficacy OR vs Drop-out OR
plot(1 ~ 1,
     type = "n",
     xlim = range(c(Drop.out.OR.lower,
                    Drop.out.OR.upper)),
     ylim = range(c(Efficacy.OR.lower,
                    Efficacy.OR.upper)),
     xlab = "Drop-out OR",
     ylab = "",
     yaxt = "n")
segments(x0 = Drop.out.OR.lower,
         x1 = Drop.out.OR.upper,
         y0 = Efficacy.OR.mean,
         y1 = Efficacy.OR.mean,
         col = 1:6,
         lw = c(2,2,2,2,2,3))
segments(x0 = Drop.out.OR.mean,
         x1 = Drop.out.OR.mean,
         y0 = Efficacy.OR.upper,
         y1 = Efficacy.OR.lower,
         col = 1:6,
         lw = c(2,2,2,2,2,3))

# 3. Drop-out OR vs RD
plot(1 ~ 1,
     type = "n",
     xlim = range(c(RD.lower,
                    RD.upper)),
     ylim = range(c(Drop.out.OR.lower,
                    Drop.out.OR.upper)),
     xlab = "RD (%)",
     ylab = "Drop-out OR")
segments(x0 = RD.lower,
         x1 = RD.upper,
         y0 = Drop.out.OR.mean,
         y1 = Drop.out.OR.mean,
         col = 1:6,
         lw = c(2,2,2,2,2,3))
segments(x0 = RD.mean,
         x1 = RD.mean,
         y0 = Drop.out.OR.upper,
         y1 = Drop.out.OR.lower,
         col = 1:6,
         lw = c(2,2,2,2,2,3))
graphics.off()