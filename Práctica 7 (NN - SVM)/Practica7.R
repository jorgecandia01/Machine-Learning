setwd("/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/Práctica 7 (NN - SVM)")

##### PREPROCESADO DE LOS DATOS #####
library(ISLR)
library(tidyverse)      #data manipulation and visualization
library("psych")  # For multi.hist
library("corrplot") #For corrplot
library("modelr")  # For add_predictions
library(varhandle)
library(dummies)

#data(package = 'ISLR') #Inspecciono datasets de ISLR e importo Wage
wages <- as.data.frame(Wage)


#Preprocesado. Todas las variables discretas vienen ya en factor
any(is.na(wages)) #No hay NAs en el dataframe
summary(wages)
str(wages)
wages <- wages[,-c(6,10)] #Sólo hay una región y quito tb logwage (explico wage)
wages <- dummy.data.frame(wages) #Convierto factores a dummies


#Partición de los datos para training y test
set.seed(100)

inputs<-wages[,-c(23)]
target<-wages[,c(23)]

#Nomalizo variable 1, 2; La explicada (9) ya está en target sóla
variables_normalizadas<-normalizeData(inputs[,c(1,2)], type="0_1")
inputs_norm<-inputs

inputs_norm<-normalizeData(inputs, type="0_1")
target_norm<-normalizeData(target, type="0_1")

sets<-splitForTrainingAndTest(inputs_norm,target_norm,ratio=0.15) #Función mazo útil de RSNNS






############### SE REALIZARÁ EL PERCEPTRÓN MULTICAPA (MLP) CON LA LIBRERÍA RSNNS ###############
library(RSNNS)      # for MLP training
library(ROCR)       # for ROC curves

### Creación del modelo de MLP
mlp_model<-mlp(sets$inputsTrain, #Separo variables explicativas de la explicada
               sets$targetsTrain, 
               size=c(9,5,3), #number of units in the hidden layer(s) // 2 hidden layers, de 5 y 3 neuronas
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
plotIterativeError(mlp_model) #Error de training (rojo) VS error de test (negro) AL REVES 
#Plotting error for test
plotRegressionError(sets$targetsTest,pred_ts_norm,pch=3)
plotROC(pred_ts_norm, sets$inputsTest)


# Weights and other information of the mlp
weightMatrix(mlp_model)
extractNetInfo(mlp_model)
summary(mlp_model)
print(mlp_model)

###############################
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

#Histogram of residuals 
ggplot(data=as.data.frame(error_ts), mapping= aes(x=error_ts))+
  geom_histogram(binwidth=0.9, col=c('blue'))

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
















############### SE REALIZARÁ LA SUPPORT VECTOR REGRESSION (SVR) CON LA LIBRERÍA e1071 ###############
library(e1071)
#Hago un bucle para ver los mejores hiperparámetros // CUIDADO TARDA MUCHO
svr_train<-tune("svm", wage~.,
                data=wages,
                kernel="radial",
                ranges=list(
                  epsilon=seq(0,1,0.1),
                  cost=c(0.01, 0.1, 1, 5, 10, 20),
                  gamma=c(0.1, 0.5, 1, 2, 5, 10)))


summary(svr_train)

#Guardo el mejor modelo
best_model <- svr_train$best.model
summary(best_model)

# Plotting error versus cost
ggplot(data=svr_train$performances, aes(x=cost, y=error, color=factor(epsilon)))+
  geom_line()+
  geom_point()+
  labs(title="Classification error vs  C & epsilon")+
  theme_bw()+
  theme(legend.position ="bottom")




####################
###Predicciones -> MSE y R2, cálculo y exloración de errores
pred_svr_tr <- predict(best_model, wages)
error_svr_tr<- wages$wage - pred_svr_tr


#### Exploración de errores

#Histogram of training error
ggplot(data=as.data.frame(error_svr_tr), mapping= aes(x=error_svr_tr))+
  geom_histogram(binwidth=0.9, col=c('blue'))

# Valores reales VS predichos TRAINING
plot(wages$wage,type = "p",col = "red", xlab = "Sample", 
     ylab = "medv Value", 
     main = "Training: Real (red) - Predicted (blue)") #Valor real

lines(pred_svr_tr, type = "p", col = "blue") #Valor predicho

# Compare predictions vs real values (cuanto más cerca de la recta mejor)
plot(wages$wage,pred_svr_tr,col='red',
     main='Training: Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)


###MSE y R2

# Calculating MSE for training
MSE.nn <- sum((error_svr_tr)^2)/length(error_svr_tr)

#Estimation of R^2 for training
num<-sum((error_svr_tr)^2)
den<-sum((wages$wage-mean(wages$wage))^2)

R2<-1-(num/den)
















