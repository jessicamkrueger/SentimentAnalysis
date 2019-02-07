setwd("~/Ubiqum/UbiqumWork/Project3/small matrices")

iphone2 <- readRDS("iphone2.rds")


#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#try running model on iphone1 data set
set.seed(234)

# define an 70%/30% train/test split of the dataset
inTraining1 <- createDataPartition(iphone2$iphonesentiment, p = .70, list = FALSE)
training1 <- iphone2[inTraining1,]
testing1 <- iphone2[-inTraining1,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train kNN classification model
rf3 <- train(iphonesentiment~., data = training1, method = "rf", trControl=fitControl)


#make predictions
predrf3 <- predict(rf3, testing1)

#performace measurment
postResample(predrf3, testing1$iphonesentiment)
# output
# Accuracy     Kappa 
# 0.8866219 0.5922991 

confusionMatrix(predrf3, testing1$iphonesentiment)
#               Reference
# Prediction    1    2
#           1  410   11
#           2  428 3023




