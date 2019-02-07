setwd("~/Ubiqum/UbiqumWork/Project3/small matrices")

iphone <- readRDS("iphone01.rds")


#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#run an out of box modelto see initial what results are

set.seed(234)

# define an 70%/30% train/test split of the dataset
inTraining <- createDataPartition(iphone1$iphonesentiment, p = .70, list = FALSE)
training <- iphone1[inTraining,]
testing <- iphone1[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train kNN classification model
kknn <- train(iphonesentiment~., data = training, method = "kknn", trControl=fitControl)


#make predictions
predkknn <- predict(kknn, testing)

#performace measurment
postResample(predkknn, testing$iphonesentiment)
# Accuracy      Kappa 
# 0.42636854 0.09627936 

#confusion matrix
confusionMatrix(predkknn, testing$iphonesentiment)
# Reference
# Prediction    1    2
# 1  739 2130
# 2  102  920