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
library(neuralnet)  # for MLP training
library(caret)      # for confusion matrix and more
library(rsample)    # for data splitting

# Set a seed
set.seed(500)

library(MASS)
data <- Boston

# Check that no data is missing
apply(data,2,function(x) sum(is.na(x)))

#Creating a training and test datasets
set.seed(123)
boston_split<- initial_split(Boston, prop=0.75)
boston_train<- training(boston_split)
boston_test<- testing(boston_split)

# Fitting linear model
lm.fit <- lm(medv~., data=boston_train)
summary(lm.fit)

# Predicted data from lm
pr.lm <- predict(lm.fit,boston_test)

# Test MSE
MSE.lm <- sum((pr.lm - boston_test$medv)^2)/nrow(boston_test)

#-------------------------------------------------------------------------------
# Neural net fitting

# Scaling data for the NN. Normalization
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

# Train-test split
train_ <- scaled[boston_split$in_id,]
test_ <- scaled[-boston_split$in_id,]

# NN training
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + "))) #Comando interesante
nn <- neuralnet(f,data=train_,hidden=c(5,3), #Cómo sabes qué hidden escoger? en que te basas? A ojo//con bucles
                rep = 100, #n de iteraciones (epoch)
                startweights = NULL,
                lifesign = "none",
                linear.output=T)

# Visual plot of the model
plot(nn, rep="best")

# Best repetition with minimum error
best_rep<-which.min(nn$result.matrix[1,])


# Weights for the repetition with the best error
nn$weights[best_rep]

# activation function
nn$act.fct

# result.matrix: threshold, step, error, AIC, BIC and weights
#plotting the error per repetition
plot(nn$result.matrix[1,])
#threshold per repetition
plot(nn$result.matrix[2,])

# best 43    
plot(nn$result.matrix[43,])

#prediction with training set
pr.nn_training <- predict(nn, train_, rep=best_rep,all.units=FALSE)

# Results from NN are normalized (scaled)
# Descaling for comparison
ds_pr.nn_tr <- pr.nn_training*(maxs[14]-mins[14])+mins[14]

# Plot real and predicted values for training
plot(boston_train$medv,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Training: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(ds_pr.nn_tr, type = "p", col = "blue")


plot(ds_pr.nn_tr)
error_training<-boston_train$medv-ds_pr.nn_tr

#Histogram of residuals
ggplot(data=as.data.frame(error_training), mapping= aes(x=error_training))+
  geom_histogram(binwidth=0.5, col=c('blue'))

# Calculating MSE for training
MSE.nn <- sum((error_training)^2)/nrow(error_training)

#Estimation of R^2 for training
num<-sum((error_training)^2)
den<-sum((boston_train$medv-mean(data$medv))^2)

R2<-1-(num/den)
R2_adjust<-(1-(1-R2)*(nrow(boston_train)-1)/(nrow(boston_train)
                                             -ncol(boston_train)-1))

# Compare predictions vs real values
plot(boston_train$medv,ds_pr.nn_tr,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)

###########################################################
# Prediction with test dataset
pr.nn_test <- predict(nn,test_, rep=best_rep,all.units=FALSE)

# Results from NN are normalized (scaled)
# Descaling for comparison
ds_pr.nn_test <- pr.nn_test*(maxs[14]-mins[14])+mins[14]

# Plot real and predicted values for test
plot(boston_test$medv,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(ds_pr.nn_test, type = "p", col = "blue")

plot(ds_pr.nn_test)
error_test<-boston_test$medv-ds_pr.nn_test

# Calculating MSE for the test data set
MSE.nn_test <- sum((error_test)^2)/nrow(error_test)

#Estimation of R^2 for test
num<-sum((error_test)^2)
den<-sum((boston_test$medv-mean(data$medv))^2)

R2_test<-1-(num/den)
R2_adjust_test<-(1-(1-R2_test)*(nrow(boston_test)-1)/(nrow(boston_test)
                                             -ncol(boston_test)-1))


#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.5, col=c('blue'))

# Compare predictions vs real values
plot(boston_test$medv,ds_pr.nn_test,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)

# Comparing MSEs for both models in test
print(paste(MSE.lm,MSE.nn_test))

# Plot predictions
par(mfrow=c(1,2))

plot(boston_test$medv,ds_pr.nn_test,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(boston_test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

# Compare predictions on the same plot
plot(boston_test$medv,ds_pr.nn_test,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(boston_test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

