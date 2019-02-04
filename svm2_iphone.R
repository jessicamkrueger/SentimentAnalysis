setwd("~/Ubiqum/UbiqumWork/Project3/small matrices")

iphone <- read.csv("iphone_smallmatrix_labeled_8d.csv")
galaxy <- read.csv("galaxy_smallmatrix_labeled_8d.csv")



#change the sentiment scores to just positive or negative
#0 = negative
#1 = positive

iphone$iphonesentiment[iphone$iphonesentiment == 1] <- 0
iphone$iphonesentiment[iphone$iphonesentiment == 2] <- 0
iphone$iphonesentiment[iphone$iphonesentiment == 5] <- 1
iphone$iphonesentiment[iphone$iphonesentiment == 4] <- 1
iphone$iphonesentiment[iphone$iphonesentiment == 3] <- 1

ggplot(iphone, aes(iphonesentiment)) +
  geom_bar()

iphone$iphonesentiment <- as.integer(iphone$iphonesentiment)
iphone$iphonesentiment <- as.factor(iphone$iphonesentiment)
summary(iphone$iphonesentiment)

saveRDS(iphone, "iphone01.rds")

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
svm2 <- train(iphonesentiment~., data = training, method = "svmLinear", trControl=fitControl)


#make predictions
predsvm2 <- predict(svm2, testing)

#performace measurment
postResample(predsvm2, testing$iphonesentiment)
# output
Accuracy   Kappa    
0.8586209  0.4847806

#view model
svm2


#check confusion matrix
confusionMatrix(predsvm2,testing$iphonesentiment)

            Reference
Prediction    1    2
         1  371   66
         2  470 2984


