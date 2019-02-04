#find near zero variance features

nzvMetrics <- nearZeroVar(iphone1, saveMetrics = TRUE)

#create index of near zero variance features
nzv <- nearZeroVar(iphone1, saveMetrics = FALSE) 

iphone2 <- iphone1[,-nzv]
saveRDS(iphone2, "iphone2.rds")
