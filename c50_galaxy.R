setwd("~/Ubiqum/UbiqumWork/Project3/small matrices")

galaxy <- read.csv("galaxy_smallmatrix_labeled_8d.csv")

galaxy$galaxysentiment[galaxy$galaxysentiment<= 2] <- 0
galaxy$galaxysentiment[galaxy$galaxysentiment> 2] <- 1

galaxy$galaxysentiment <- as.factor(galaxy$galaxysentiment)

#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#run an out of box modelto see initial what results are

set.seed(234)

# define an 70%/30% train/test split of the dataset
inTraining <- createDataPartition(galaxy$galaxysentiment, p = .70, list = FALSE)
training <- galaxy[inTraining,]
testing <- galaxy[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train kNN classification model
gc5 <- train(galaxysentiment~., data = training, method = "C5.0", trControl=fitControl)


#make predictions
predgc5 <- predict(gc5, testing)

#performace measurment
postResample(predgc5, testing$galaxysentiment)
# Accuracy     Kappa 
# 0.9020817 0.6232090 

#performace measurment
confusionMatrix(predgc5, testing$galaxysentiment)
#               Reference
# Prediction    0    1
#           0  393   15
#           1  371 3112