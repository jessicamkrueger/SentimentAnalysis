setwd("~/Ubiqum/UbiqumWork/Project3/small matrices")

iphone <- readRDS("iphone01.rds")


#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)
library(C50)
library(inum)

#run an out of box modelto see initial what results are

set.seed(234)

# define an 70%/30% train/test split of the dataset
inTraining <- createDataPartition(iphone$iphonesentiment, p = .70, list = FALSE)
training <- iphone[inTraining,]
testing <- iphone[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(
  method = "repeatedcv", 
  number = 2, 
  repeats = 1)

#train kNN classification model
c51 <- train(iphonesentiment~., 
  data = training, 
  method = "C5.0", 
  trControl=fitControl)


#make predictions
predc51 <- predict(c51, testing)

#performace measurment
postResample(predc51, testing$iphonesentiment)
# output
# Accuracy     Kappa 
# 0.8861475 0.5913225 

#view model
c51

varImp(c51)


#check confusion matrix
confusionMatrix(predc51,testing$iphonesentiment)
#               Reference
# Prediction    1    2
#           1  413   15
#           2  428 3035

