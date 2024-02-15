# Dataset Boston
# The idea is to fit the mean price of houses (medv) in Boston 
# in function of the available variables that are the following

#CRIM - per capita crime rate by town
#ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
#INDUS - proportion of non-retail business acres per town.
#CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
#NOX - nitric oxides concentration (parts per 10 million)
#RM - average number of rooms per dwelling
#AGE - proportion of owner-occupied units built prior to 1940
#DIS - weighted distances to five Boston employment centres
#RAD - index of accessibility to radial highways
#TAX - full-value property-tax rate per $10,000
#PTRATIO - pupil-teacher ratio by town
#B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#LSTAT - % lower status of the population
#MEDV - Median value of owner-occupied homes in $1000's


library(MASS)        # for obtaining data
library(tidyverse)  # for data processing
library(rpart)      # for CART decision tree
library(rpart.plot) # for plotting CART
library(caret)      # for confusion matrix and more
library(rsample)    # for data splitting


data(Boston) # getting data
ncol(Boston)
nrow(Boston)
summary(Boston)
glimpse(Boston)
pairs(Boston)

#Creating a training and test datasets
set.seed(123)
boston_split<- initial_split(Boston, prop=0.8)
boston_train<- training(boston_split)
boston_test<- testing(boston_split)


# Creating the regression tree
tree_result<- rpart(formula=medv  ~ ., data=boston_train, 
                    method='anova')

#Resulting tree
print(tree_result)

# Alternative views
summary(tree_result)




#Fitting the plotting allowing labels in several lines
split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width = 10), collapse = "\n")
  }
  labs
}

#PLotting the tree
rpart.plot(tree_result, type=1, branch=0,tweak=1.4, 
           fallen.leaves = TRUE,
           varlen = 0, faclen = 0, split.fun=split.fun)
#Alternative for plotting the tree
prp(tree_result, faclen=3, clip.facs=TRUE, 
    split.fun=split.fun, tweak=1.1, extra=101)

################# #Prediction ############
#Poor option to plot the tree
plot(tree_result, uniform=TRUE, compress=TRUE)
text(tree_result, use.n=TRUE)

plot(tree_result, subtree=3)

#Obtainimng the decision rules from the tree
rpart.rules(tree_result, style = "tall", cover=TRUE,
            nn=TRUE, clip.facs = TRUE)


#Prediction of the training cases from the train dataset
pred_train <- predict(tree_result, newdata = boston_train)

# Plotting the real and estimated values. (p point, l lines, o both)
plot(boston_train$medv,type = "p",col = "red", xlab = "Sample", ylab = "medv Value", 
     main = "medv: Real (red) - Predicted (blue)")
lines(pred_train, type = "p", col = "blue")

#Residuals
res_train<-boston_train$medv-pred_train

# Plotting residuals. (p point, l lines, o both)
plot(res_train,type = "p",col = "red", xlab = "Sample", ylab = "Residual Value", 
     main = "Sale Residuals: Real - Predicted Values")

mse_train <-mean((res_train)^2)  ## MSE estimation

paste("The MSE of the regression tree in training is: ", 
      round(mse_train,2))


#Prediction of the test cases from the test dataset
pred_test <- predict(tree_result, newdata = boston_test)

# Plotting the real and estimated values. (p point, l lines, o both)
plot(boston_test$medv,type = "p",col = "red", xlab = "Sample", ylab = "medv Value", 
     main = "medv: Real (red) - Predicted (blue)")
lines(pred_test, type = "p", col = "blue")

#Residuals
res_test<-boston_test$medv-pred_test

# Plotting residuals. (p point, l lines, o both)
plot(res_test,type = "p",col = "red", xlab = "Sample", ylab = "Residual Value", 
     main = "Sale Residuals: Real - Predicted Values")

mse_test <-mean((res_test)^2)  ## MSE estimation

paste("The MSE of the regression tree in test is: ", 
      round(mse_test,2))


# Analysis of cp values in a table
printcp(tree_result, digits=4)


# Error evolution with increasing number of nodes
plotcp(tree_result, lty=2 , col="red", upper="size" )
plotcp(tree_result, lty=2 , col="red", upper="splits" )

###################################################
#Prunning analysis with cp 0.01
###################################################
tree_pruned<- prune(tree_result, cp=0.01)
rpart.plot(tree_pruned, type=1, branch=0,tweak=1.3, 
           fallen.leaves = TRUE,
           varlen = 0, faclen = 0, split.fun=split.fun)


#Prediction of the training cases from the train dataset PRUNING
pred_train <- predict(tree_pruned, newdata = boston_train)


# Plotting the real and estimated values. (p point, l lines, o both)
plot(boston_train$medv,type = "p",col = "red", xlab = "Sample", ylab = "medv Value", 
     main = "medv: Real (red) - Predicted (blue)")
lines(pred_train, type = "p", col = "blue")

#Residuals
res_train<-boston_train$medv-pred_train

# Plotting residuals. (p point, l lines, o both)
plot(res_train,type = "p",col = "red", xlab = "Sample", ylab = "Residual Value", 
     main = "Sale Residuals: Real - Predicted Values")

mse_train <-mean((res_train)^2)  ## MSE estimation

paste("The MSE of the regression tree in training is: ", 
      round(mse_train,2))


#Prediction of the test cases from the test dataset
pred_test <- predict(tree_result, newdata = boston_test)

# Plotting the real and estimated values. (p point, l lines, o both)
plot(boston_test$medv,type = "p",col = "red", xlab = "Sample", ylab = "medv Value", 
     main = "medv: Real (red) - Predicted (blue)")
lines(pred_test, type = "p", col = "blue")

#Residuals
res_test<-boston_test$medv-pred_test

# Plotting residuals. (p point, l lines, o both)
plot(res_test,type = "p",col = "red", xlab = "Sample", ylab = "Residual Value", 
     main = "Sale Residuals: Real - Predicted Values")

mse_test <-mean((res_test)^2)  ## MSE estimation

paste("The MSE of the regression tree in test is: ", 
      round(mse_test,2))

