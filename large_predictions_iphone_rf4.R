#apply model to large matrix data

setwd("~/Ubiqum/UbiqumWork/Project3/small matrices")
rf4 <- readRDS("rf4.rds")

setwd("~/Ubiqum/UbiqumWork/Project3/largeMatrix")
large <- read.csv("largeMatrixiPhone.csv")

#make predictions
predLargeMatrix <- predict(rf4, large)

summary(predLargeMatrix)
# 1     2 
# 9307 11956 


#apply model to large matrix data


saveRDS(grf, "grf.rds")
setwd("~/Ubiqum/UbiqumWork/Project3/largeMatrix")
largeg <- read.csv("largeMatrixgalaxy.csv")

#make predictions
predLargeMatrixg <- predict(grf, largeg)

summary(predLargeMatrixg)
# 0     1 
# 9334 11929 