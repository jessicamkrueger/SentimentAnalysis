setwd("~/Ubiqum/UbiqumWork/Project3/small matrices")

iphone <- read.csv("iphone_smallmatrix_labeled_8d.csv")
galaxy <- read.csv("galaxy_smallmatrix_labeled_8d.csv")


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
knn1 <- train(iphonesentiment~., data = training, method = "knn", trControl=fitControl)


#make predictions
predknn1 <- predict(knn1, testing)

#performace measurment
postResample(predknn1, testing$iphonesentiment)
# output


#view kNN model
knn1


#check confusion matrix
confusionMatrix(predknn1,testing$iphonesentiment)
