setwd("~/Ubiqum/UbiqumWork/Project3/small matrices")

iphone3 <- readRDS("iphone3")
iphone4 <- select(iphone3, -ID)


#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#try running model on iphone1 data set
set.seed(234)

# define an 70%/30% train/test split of the dataset
inTraining1 <- createDataPartition(iphone4$iphonesentiment, p = .70, list = FALSE)
training1 <- iphone4[inTraining1,]
testing1 <- iphone4[-inTraining1,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train kNN classification model
rf4 <- train(iphonesentiment~., data = training1, method = "rf", trControl=fitControl)
saveRDS(rf4, "rf4.rds")

#make predictions
predrf4 <- predict(rf4, testing1)

#performace measurment
postResample(predrf4, testing1$iphonesentiment)
# output
# Accuracy     Kappa 
# 0.9017264 0.6181418

confusionMatrix(predrf4, testing1$iphonesentiment)
# Reference
# Prediction    1    2
# 1  375   14
# 2  356 3020




