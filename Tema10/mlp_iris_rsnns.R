#Data Set Information:
  
#This is perhaps the best known database to be found in the 
#pattern recognition literature. The data set contains 3 classes 
# of 50 instances each, where each class refers to a type of iris plant. One class is linearly separable from the other 2; the latter are NOT linearly separable from each other. 

#Predicted attribute: class of iris plant.

#Attribute Information:
  
# 1. sepal length in cm 
#2. sepal width in cm 
#3. petal length in cm 
#4. petal width in cm 
#5. class: 
#-- Iris Setosa 
#-- Iris Versicolour 
#-- Iris Virginica

# Attribute Information: All attributes are continuous 
#NOTE: 1st attribute is class identifier (1-3


library(tidyverse)  # for data processing
library(RSNNS)  # for MLP training
library(ggplot2)    #for plotting
library(caret)     #for confusion matrix


# Data collection from source
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
download.file(url, destfile = 'iris.csv', method = 'auto')

data<- read.csv('iris.csv', header = FALSE, sep=',')

# Viewing data collected
head(data)
str(data)

#assigning names for attributes from iris.names
names(data) <- c('sepal_length','sepal_width','petal_length',
                 'petal_width','class')

summary(data)

#scatterplots
pairs(data[1:4], col=data$class)

#plotting some specific relationships for cluster discovering
qplot(sepal_length, sepal_width, data=data, 
      color=factor(data$class), geom=c("point","smooth"), 
      main = "Sepal length and width in class")


qplot(petal_length, petal_width, data=data, color=factor(data$class), 
      geom=c("point","smooth"), 
      main = "petal length and width in class")

#Defining inputs and output of the model
irisValues <- data[,1:4]
irisTargets <- data[,5]

#DECODING labes of class
irisDecTargets <- decodeClassLabels(irisTargets)

# creation of training and test datasets
iris <- splitForTrainingAndTest(irisValues, irisDecTargets, ratio=0.15)

index <- sample(nrow(data), round(0.85*nrow(data)))
iris$inputsTrain <- irisValues[index,] 
iris$targetsTrain <-irisDecTargets[index,]
iris$inputsTest <- irisValues[-index,]
iris$targetsTest <- irisDecTargets[-index,]

#Normalization of variables
iris_norm <- normTrainingAndTestSet(iris)

#training the neural network
ann_model <- mlp(iris_norm$inputsTrain, iris_norm$targetsTrain, 
                 size = 5,
                 learnFuncParams = 0.1, maxit = 100,
                 inputsTest = iris_norm$inputsTest, 
                 targetsTest = iris_norm$targetsTest)

plotIterativeError(ann_model)

#prediction with training set
pr.nn_tr <- predict(ann_model, iris_norm$inputsTrain)
pr.nn_tr_round<-as.data.frame(round(pr.nn_tr))

pred_train<-max.col(pr.nn_tr_round)
real_train<-max.col(iris_norm$targetsTrain)

library(caret)
print(real_train)
print(pred_train)
caret::confusionMatrix(table(real_train,pred_train))


# Plot real and predicted values for training
plot(real_train,type = "p",col = "red", xlab = "Sample", 
     ylab = "wine type", 
     main = "Training: Real (red) - Predicted (blue)")
lines(pred_train, type = "p", col = "blue")


#prediction with TEST set
pr.nn_ts <- predict(ann_model, iris_norm$inputsTest)
pr.nn_ts_round<-as.data.frame(round(pr.nn_ts))

pred_ts<-max.col(pr.nn_ts_round)
real_ts<-max.col(iris$targetsTest)

# Plot real and predicted values for training
plot(real_ts,type = "p",col = "red", xlab = "Sample", 
     ylab = "wine type", 
     main = "Training: Real (red) - Predicted (blue)")
lines(pred_ts, type = "p", col = "blue")


print(real_ts)
print(pred_ts)
caret::confusionMatrix(table(real_ts,pred_ts))

weightMatrix(ann_model)




