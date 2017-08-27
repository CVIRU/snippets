require(data.table)
dt1 <- fread("data/Health App/steps.csv")
dt1$date <- as.Date(substr(x = dt1$endDate, 
                           start = 1, 
                           stop = 10),
                    format = "%m/%d/%Y") 

dt2 <- aggregate(dt1$value,
                 by = list(dt1$date),
                 FUN = sum)
plot(dt2)

dt2$Group.1 <- factor(dt2$Group.1,
                      levels = as.character(dt2$Group.1))
plot(dt2$x ~ dt2$Group.1)
