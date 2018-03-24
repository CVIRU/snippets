# DS 10/22/2016
require(rgl)

########################################
# Data
source("Dropout_OR_by_group.R")
# 1. Dropout OR
# 2. HF OR
# 3. Dropout Risk Difference
# 4. HF Risk Difference

plot(1 ~ 1,
     type = "n",
     xlim = range(c(gDO.upper,
                    gDO.lower)),
     ylim = range(c(gOR.upper,
                    gOR.lower)),
     xlab = "Dropout OR",
     ylab = "HF OR")
segments(x0 = gDO.lower,
         x1 = gDO.upper,
         y0 = gOR.mean,
         y1 = gOR.mean,
         col = 1:5)
segments(x0 = gDO.mean,
         x1 = gDO.mean,
         y0 = gOR.upper,
         y1 = gOR.lower,
         col = 1:5)
text(levels(dClass),
     x = gDO.mean,
     y = gOR.mean,
     col = 1:5)

df1 <- data.frame(type = c(levels(dClass),
                           "Average"),
                  do.or.ul = c(gDO.upper,
                               do.upper),
                  do.or.ll = c(gDO.lower,
                               do.lower),
                  do.or.mu = c(gDO.mean,
                               do.mean),
                  
                  hf.or.ul = c(gOR.upper,
                               or.upper),
                  hf.or.ll = c(gOR.lower,
                               or.lower),
                  hf.or.mu = c(gOR.mean,
                               or.mean),
                  
                  do.rd.ul = c(gDO.upper2,
                               do.upper2),
                  do.rd.ll = c(gDO.lower2,
                               do.lower2),
                  do.rd.mu = c(gDO.mean2,
                               do.mean2),
                  
                  hf.rd.ul = c(gRD.upper,
                               rd.upper),
                  hf.rd.ll = c(gRD.lower,
                               rd.lower),
                  hf.rd.mu = c(gRD.mean,
                               rd.mean))
df1
save(df1, file = "df1.RData")

#################################################
# 3D plots
load("df1.RData")
size = 10
col = 1:nrow(df1)

a <- cube3d(trans = identityMatrix(),
            col = "blue",
            alpha = 0.2, 
            add = T)
colnames(a$vb) <- LETTERS[1:8]
rownames(a$vb) <- c("DO.OR", 
                    "HF.OR",
                    "DO.RD",
                    "None")

open3d()

plot3d(x = df1$do.or.mu,
       y = df1$hf.or.mu,
       z = df1$do.rd.mu,
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

foo.v(a = a, 
      lb = t(df1[, c("do.or.ll",
                     "hf.or.ll",
                     "do.rd.ll")]), 
      ub = t(df1[, c("do.or.ul",
                     "hf.or.ul",
                     "do.rd.ul")]),
      j = col)

title3d(xlab = "DO.OR",
        ylab = "HF.OR",
        zlab = "DO.RD")
texts3d(x = df1$do.or.mu,
        y = df1$hf.or.mu,
        z = df1$do.rd.mu,
        text = df1$type)

for (i in 1:6) {
  segments3d(x = c(df1$do.or.ll[i], df1$do.or.ul[i]),
             y = rep(df1$hf.or.mu[i], 2),
             z = rep(df1$do.rd.mu[i], 2),
             lw = 3,
             col = col[i])
  segments3d(x = rep(df1$do.or.mu[i], 2),
             y = c(df1$hf.or.ll[i], df1$hf.or.ul[i]),
             z = rep(df1$do.rd.mu[i], 2),
             lw = 3,
             col = col[i])
  segments3d(x = rep(df1$do.or.mu[i], 2),
             y = rep(df1$hf.or.mu[i], 2),
             z = c(df1$do.rd.ll[i], df1$do.rd.ul[i]),
             lw = 3,
             col = col[i])
}

aspect3d(1,1,1)

# play3d(spin3d())
# NOTE
movie3d(f = spin3d(),
        duration = 10,
        convert = TRUE,
        dir = paste(getwd(),
                    "tmp",
                    sep = "/"))