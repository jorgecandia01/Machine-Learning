library(datasets)
library(MASS)
library(caret)


data <- Boston
DP = caret::createDataPartition(Boston$medv, p = 0.75, list = F)

train = Boston[DP, ]
test = Boston[-DP, ]

colnames(train) = colnames(Boston)
colnames(test) = colnames(Boston)

# Scaling data for the NN. Normalization
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

# Train-test split
train_ <- scaled[DP,]
test_ <- scaled[-DP,]

set.seed(244)
mlp = caret::train(medv ~ .,data = train_,method = "mlp",
      trControl = trainControl(method = "cv",
      number = 3,
      savePredictions = "final"),
      preProc = c("center", "scale"),
      tuneGrid = expand.grid(size = 1:3),
      linOut = TRUE,
      metric = "RMSE")

#prediction with training set
pr.nn_training <- as.numeric(caret::predict.train(mlp, train_[1:13]))

# Results from NN are normalized (scaled)
# Descaling for comparison
ds_pr.nn_tr <- pr.nn_training*(maxs[14]-mins[14])+mins[14]

# Plot real and predicted values for training
plot(train$medv,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Training: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(ds_pr.nn_tr, type = "p", col = "blue")


plot(ds_pr.nn_tr)
error_training<-train$medv-ds_pr.nn_tr

#Histogram of residuals
ggplot(data=as.data.frame(error_training), mapping= aes(x=error_training))+
  geom_histogram(binwidth=0.5, col=c('blue'))

# Calculating MSE for training
MSE.nn <- sum((error_training)^2)/nrow(train)

#Estimation of R^2 for training
num<-sum((error_training)^2)
den<-sum((train$medv-mean(data$medv))^2)

R2<-1-(num/den)
R2_adjust<-(1-(1-R2)*(nrow(train)-1)/(nrow(train)
                                             -ncol(train)-1))

# Compare predictions vs real values
plot(train$medv,ds_pr.nn_tr,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)

#PREDICTION WITH TEST dataset
Yp = caret::predict.train(mlp, test[, 1:13])
summary(Yp)

#prediction with training set
pr.nn_test <- as.numeric(caret::predict.train(mlp, test_[1:13]))

# Results from NN are normalized (scaled)
# Descaling for comparison
ds_pr.nn_ts <- pr.nn_test*(maxs[14]-mins[14])+mins[14]

# Plot real and predicted values for training
plot(test$medv,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(ds_pr.nn_ts, type = "p", col = "blue")


plot(ds_pr.nn_tr)
error_test<-test$medv-ds_pr.nn_ts

#Histogram of residuals
ggplot(data=as.data.frame(error_test), mapping= aes(x=error_test))+
  geom_histogram(binwidth=0.5, col=c('blue'))

# Calculating MSE for training
MSE.nn.test <- sum((error_test)^2)/nrow(test)

#Estimation of R^2 for training
num<-sum((error_test)^2)
den<-sum((test$medv-mean(data$medv))^2)

R2_test<-1-(num/den)
R2_adjust_test<-(1-(1-R2_test)*(nrow(test)-1)/(nrow(test)
                                      -ncol(test)-1))

# Compare predictions vs real values
plot(test$medv,ds_pr.nn_ts,col='red',
     main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


 