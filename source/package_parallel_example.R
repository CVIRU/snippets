# |----------------------------------------------|
# | Project: Example of using package "parallel" |
# | Script:  Example of using package "parallel" |
# | Depends: parallel                            |
# | Author:  Davit Sargsyan                      | 
# | Created: 01/12/2018                          |
# | Reference:                                   |
# |----------------------------------------------|
# Header----
require(parallel)

# Data----
dt1 <- data.frame(matrix(rnorm(1000000),
                         nrow = 100))

# Pearson's correlations with single-tread processing----
system.time({
  out <- lapply(dt1,
                function(a) {
                  return(cor = round(cor(x = dt1[, 1], 
                                         y = a,
                                         use = "pairwise.complete.obs"),
                                     3))
                })
})
# user  system elapsed 
# 0.90    0.02    0.92

v1 <- do.call("c", out)
gc()

# Make cluster----
ncores <- detectCores()
ncores
cl <- makeCluster(getOption("cl.cores", ncores - 1))
cl
# socket cluster with 7 nodes on host ‘localhost’

# Make data available to the cluster
clusterExport(cl = cl,
              varlist = "dt1")

# Pearson's correlations with parallel processing----
system.time({
  out <- parLapply(cl = cl,
                   X = 1:ncol(dt1),
                   fun = function(i) {
                     return(cor = round(cor(x = dt1[, 1], 
                                            y = dt1[, i],
                                            use = "pairwise.complete.obs"),
                                        3))
                   })
})
# user  system elapsed 
# 0.01    0.01    0.43 

stopCluster(cl)
gc()

v2 <- do.call("c", out)

plot(v1 ~ v2)