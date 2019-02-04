setwd("~/Ubiqum/UbiqumWork/Project3/small matrices")

iphone <- read.csv("iphone_smallmatrix_labeled_8d.csv")
galaxy <- read.csv("galaxy_smallmatrix_labeled_8d.csv")

iphone$iphonesentiment <- as.factor(iphone$iphonesentiment)

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
svm1 <- train(iphonesentiment~., data = training, method = "svmLinear", trControl=fitControl)


#make predictions
predsvm1 <- predict(svm1, testing)

#performace measurment
postResample(predsvm1, testing$iphonesentiment)
# output


#view model
svm1


#check confusion matrix
confusionMatrix(predsvm1,testing$iphonesentiment)
