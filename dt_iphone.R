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
inTraining <- createDataPartition(iphone$iphonesentiment, p = .70, list = FALSE)
training <- iphone[inTraining,]
testing <- iphone[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train kNN classification model
dt <- train(iphonesentiment~., data = training, method = "rpart", trControl=fitControl)


#make predictions
preddt <- predict(dt, testing)

#performace measurment
postResample(preddt, testing$iphonesentiment)
# output
Accuracy     Kappa 
0.8699563    0.5467349 

#view model
dt


#check confusion matrix
confusionMatrix(preddt,testing$iphonesentiment)

              Reference
Prediction    1    2
         1  410   75
         2  431 2975

# Load rpart and rpart.plot
library(rpart)
library(rpart.plot)
# Create a decision tree model
tree <- rpart(iphonesentiment~., data=iphone, cp=.02)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
