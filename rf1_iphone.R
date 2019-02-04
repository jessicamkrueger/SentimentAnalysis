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
rf1 <- train(iphonesentiment~., data = training, method = "rf", trControl=fitControl)


#make predictions
predrf1 <- predict(rf1, testing)

#performace measurment
postResample(predrf1, testing$iphonesentiment)
# output
# Accuracy     Kappa 
# 0.8884605    0.6023327 

#view model
rf1

varImp(rf1)
Overall
# iphone        100.000
# htcphone       33.858
# samsunggalaxy  10.597
# iphoneperpos    7.764
# sonyxperia      6.026
# iphoneperneg    5.556
# iphonedispos    5.025
# htccampos       4.896
# iphonedisneg    4.807
# iphonecampos    4.673
# iphoneperunc    4.249
# iphonedisunc    4.047
# iphonecamneg    2.640
# htcdispos       2.404
# iphonecamunc    2.301
# htcperpos       1.932
# sonyperpos      1.874
# samsungcampos   1.539
# ios             1.087
# samsungdisneg   1.051

#check confusion matrix
confusionMatrix(predrf1,testing$iphonesentiment)
#               Reference
# Prediction    1    2
#           1  424   17
#           2  417 3033


#error analysis - create data frame with predictions and actual sentiment
testing$prediction <- predrf1

testing$accurate <- testing$prediction == testing$iphonesentiment
ggplot(testing, aes(accurate)) +
  geom_bar()

#explore the errors visually to find patterns
rf1_errors <- filter(testing, accurate == FALSE)
rf1_true <- filter(testing, accurate == TRUE)

ggplot(rf1_errors, aes(iphone)) +
  geom_bar()

ggplot(rf1_true, aes(iphone)) +
  geom_bar()

ggplot(rf1_errors, aes(htcphone)) +
  geom_bar()
ggplot(rf1_true, aes(htcphone)) +
  geom_bar()

ggplot(rf1_errors, aes(samsunggalaxy)) +
  geom_bar()
ggplot(rf1_true, aes(samsunggalaxy)) +
  geom_bar()


ggplot(rf1_errors, aes(iphoneperpos)) +
  geom_bar()
ggplot(rf1_true, aes(iphoneperpos)) +
  geom_bar()

ggplot(rf1_errors, aes(iphonedisneg)) +
  geom_bar()

#filter out observations where dispos is greater than 30 
#but sentiment is negative. 11 observations total. 
iphone1 <- filter(iphone, !(iphonedispos > 30 & iphonesentiment == 1))
iphone1 <- filter(iphone1, !(iphoneperpos > 30 & iphonesentiment == 1))


#filter out observations where perneg is greater than 14 but sentiment is 
#positive. 51 observations total
iphone1 <- filter(iphone1, !(iphoneperneg > 14 & iphonesentiment == 2))

#filter out rows with 12 perpos and 15 perneg 
#(already gone from previous filters)
iphone1 <- filter(iphone1, !(iphoneperneg > 14 & iphonesentiment == 2))


#try running model on iphone1 data set
set.seed(234)

# define an 70%/30% train/test split of the dataset
inTraining1 <- createDataPartition(iphone1$iphonesentiment, p = .70, list = FALSE)
training1 <- iphone[inTraining1,]
testing1 <- iphone[-inTraining1,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train kNN classification model
rf2 <- train(iphonesentiment~., data = training1, method = "rf", trControl=fitControl)


#make predictions
predrf2 <- predict(rf2, testing1)

#performace measurment
postResample(predrf2, testing1$iphonesentiment)
# output
# Accuracy     Kappa 
# 0.8906965 0.6145243

confusionMatrix(predrf2, testing1$iphonesentiment)
#               Reference
# Prediction    1    2
#           1  442   12
#           2  418 3062

#review errors
rf2_errors <- testing1
rf2_errors$prediction <- predrf2
rf2_errors$accurate <- rf2_errors$prediction == rf2_errors$iphonesentiment

postResample(rf2_errors$prediction, rf2_errors$iphonesentiment)
