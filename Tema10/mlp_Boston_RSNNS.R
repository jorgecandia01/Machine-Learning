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


library(MASS)       # for obtaining data
library(tidyverse)  # for data processing
library(RSNNS)      # for MLP training
library(ROCR)       # for ROC curves

data <- Boston


#Creating a training and test datasets
inputs<-data[,1:13]
target<-data[,14]

#Nomalization
inputs_norm<-normalizeData(inputs, type="0_1")
target_norm<-normalizeData(target, type="0_1")

sets<-splitForTrainingAndTest(inputs_norm,target_norm,ratio=0.15) #Función mazo útil de RSNNS



### Creación del modelo de MLP
mlp_model<-mlp(sets$inputsTrain, #Separo variables explicativas de la explicada
               sets$targetsTrain, 
               size=c(4,3), #number of units in the hidden layer(s) // 2 hidden layers, de 5 y 3 neuronas
               initFunc="Randomize_Weights", #Pesos aleatorios al principio
               initFuncParams=c(-0.3, 0.3), # the parameters for the initialization function
               learnFunc="Std_Backpropagation", #Método de aprendizaje -> backpropagation
               learnFuncParams=c(0.2, 0.0), # the parameters for the learning function ??????
               maxit = 450, #n batches?
               updateFunc="Topological_Order", # ????
               hiddenActFunc="Act_Logistic", # Función de activacion -> sigmoide/logistic
               linOut=TRUE, #sets the activation function of the output units to linear or logistic 
               inputsTest = sets$inputsTest, #Meto datos de test ya desde aquí
               targetsTest = sets$targetsTest)



pred_ts_norm <- predict(mlp_model, sets$inputsTest) #Predicciones de test
plotIterativeError(mlp_model) #Error de training (rojo) VS error de test (negro)
#Plotting error for test
plotRegressionError(sets$targetsTest,pred_ts_norm,pch=3)
plotROC(pred_ts_norm, sets$inputsTest)


# Weights and other information of the mlp
weightMatrix(mlp_model)
extractNetInfo(mlp_model)
summary(mlp_model)
print(mlp_model)

#########################################
###Predicciones de TRAINIG -> MSE y R2 ;Denormalización y cálculo de errores
pred_tr_norm <- predict(mlp_model, sets$inputsTrain)

pred_tr_denorm <-denormalizeData(pred_tr_norm, getNormParameters(target_norm))
target_tr_denorm<- denormalizeData(sets$targetsTrain, getNormParameters(target_norm)) ## Lo mismo q target

error_tr<-target_tr_denorm - pred_tr_denorm


#### Exploración de errores

#Histogram of training error
ggplot(data=as.data.frame(error_tr), mapping= aes(x=error_tr))+
  geom_histogram(binwidth=0.5, col=c('blue'))

# Valores reales VS predichos TRAINING
plot(target_tr_denorm,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Training: Real (red) - Predicted (blue)") #Valor real

lines(pred_tr_denorm, type = "p", col = "blue") #Valor predicho

# Compare predictions vs real values (cuanto más cerca de la recta mejor)
plot(target_tr_denorm,pred_tr_denorm,col='red',
     main='Training: Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


###MSE y R2

# Calculating MSE for training
MSE.nn <- sum((error_tr)^2)/nrow(error_tr)

#Estimation of R^2 for training
num<-sum((error_tr)^2)
den<-sum((target_tr_denorm-mean(sets$targetsTrain))^2)

R2<-1-(num/den)
R2_adjust<-(1-(1-R2)*(nrow(target_tr_denorm)-1)/(nrow(target_tr_denorm)-ncol(target_tr_denorm)-1))



#Plotting error for training
plotRegressionError(sets$targetsTrain,pred_tr_norm,pch=3)
plotROC(pred_tr_norm, sets$targetsTrain)

###########################################################
###Predicciones de TEST -> MSE y R2 ;Denormalización y cálculo de errores
pred_ts_norm <- predict(mlp_model, sets$inputsTest) #Mismo cálculo que antes

pred_ts_denorm <-denormalizeData(pred_ts_norm, getNormParameters(target_norm))
target_ts_denorm<- denormalizeData(sets$targetsTest, getNormParameters(target_norm))

error_ts<-target_ts_denorm-pred_ts_denorm


#### Exploración de errores

#Histogram of residuals option 2
ggplot(data=as.data.frame(error_ts), mapping= aes(x=error_ts))+
  geom_histogram(binwidth=0.4, col=c('blue'))

# Valores reales VS predichos TEST
plot(target_ts_denorm,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Test: Real (red) - Predicted (blue)")
lines(pred_ts_denorm, type = "p", col = "blue")

# Compare predictions vs real values (cuanto más cerca de la recta mejor)
plot(target_ts_denorm,pred_ts_denorm,col='red',
     main='Test Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


###MSE y R2

# Calculating MSE for the test data set
MSE.nn_ts <- sum((error_ts)^2)/nrow(error_ts)

#Estimation of R^2 for training
num<-sum((error_ts)^2)
den<-sum((target_ts_denorm-mean(sets$targetsTest))^2)

R2_ts<-1-(num/den)
R2_adjust_ts<-(1-(1-R2_ts)*(nrow(target_ts_denorm)-1)/(nrow(target_ts_denorm)-ncol(target_ts_denorm)-1))










