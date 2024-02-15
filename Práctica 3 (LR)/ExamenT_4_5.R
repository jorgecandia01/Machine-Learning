library("psych")  # for multi-hist
library(aod)
library(ggplot2)
library(vcd) # For mosaic
library(ROCR) #For ROC curve and lift chart
library("car")  # For VIF
library("modelr")  # For add_predictions
library(varhandle)
library("corrplot")

setwd('/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/E4')



##### PASO 1 #####
data <- read.csv("german_credit.csv")

any(is.na(data)) #No hay NAs

summary(data) #Quick view of the variables
str(data)

for (i in 1:(length(data))){
  data[[i]] <- factor(data[[i]])
}

data$Duration.of.Credit..month. <- as.integer(data$Duration.of.Credit..month.)
data$Credit.Amount <- as.integer(data$Credit.Amount)
data$Age..years. <- as.integer(data$Age..years.)

pairs(~Creditability+Duration.of.Credit..month.+Credit.Amount+Age..years., 
      main='scatterplots', col=c('blue'), data=data)



##### PASO 2 #####
log_modelC <-glm(Creditability~data[[2]]+data[[3]]*data[[4]]+data[[5]]+data[[7]]+data[[11]]
                 , family="binomial", data=data)
#Hecho con índices de data ya que las variables son muy largas, abajo la traducción
log_modelC <-glm(Creditability~Account.Balance+
                   Duration.of.Credit..month.*Payment.Status.of.Previous.Credit
                 +Purpose+Value.Savings.Stocks+Guarantors, family="binomial", data=data)
summary(log_modelC)

#NO HACEN CASI NADA/CONTRAPRODUCENTES: 6,8,9,10,12,13,15,16,17,19,21 CREDITAMOUNT??


#Test of fit
#p value of degrees of freedom
with(log_modelC, pchisq(null.deviance -deviance, df.null-df.residual, lower.tail=FALSE))
#The model fits significantly better than an empty model

#####Wald tests
wald.test(b=coef(log_modelC), Sigma=vcov(log_modelC), Terms=2:4)#Account.Balance is significant
wald.test(b=coef(log_modelC), Sigma=vcov(log_modelC), Terms=10:18)#Purpose is significant
wald.test(b=coef(log_modelC), Sigma=vcov(log_modelC), Terms=21:22)#Guarantors is significant


#predicted <- data.frame(p_creditability=log_modelC$fitted.values, creditability=data$Creditability)
#Confussion con P arbitraria
predictions<-ifelse(test=log_modelC$fitted.values>0.5,yes=1, no=0)

confusion_matrix<-table(log_modelC$model$Creditability,predictions,
                        dnn=c("observations", "predictions"))

confusion_matrix

#plotting the confussion matrix
mosaic(confusion_matrix,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))






# PASO 3: Capacidad de predicción
#Setteo un seed por si quiero repetir el estudio con exactamente las mismas muestras
set.seed(100)

#Creo un index con 800 números aleatorios del 0 al número de muestras de data sin repetirse
index <- sample(nrow(data), 0.8*nrow(data), replace = F)
#Con el, hago la partición aleatoria de data para crear el trainSet y el testSet
trainSet <- data[index,]
testSet <- data[-index,]

#Entreno el modelo con trainSet, los datos de testSet no se contemplan en el modelo
train_modelC <- glm(Creditability~Account.Balance+
                   Duration.of.Credit..month.*Payment.Status.of.Previous.Credit
                 +Purpose+Value.Savings.Stocks+Guarantors, family="binomial", data=trainSet)
summary(train_modelC) #Baja mucho el AIC y las deviances pero por tener muchas menos filas
summary(log_modelC) #Summary del modelo principal

#Con el modelo de entrenamiento, hago predicciones de los datos de test
testPredictions <- predict(train_modelC, testSet, type="response")
testPredictions <- as.numeric(testPredictions)

#Hago una matriz de confusión para ver si tiene la distribución esperada (misma p que la primera)
# p=0.5 => Si la predicción es mayor de 0.5, se cuenta el dato  como 1, si es menor o igual, 0
predictions<-ifelse(test=testPredictions>0.5,yes=1, no=0)
confusion_matrixTest<-table(testSet$Creditability,predictions,
                       dnn=c("observations", "predictions"))

confusion_matrixTest #Miro la tabla de predicciones
#La plotteo para ver los números gráficamente en forma de superficies
mosaic(confusion_matrixTest,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))

#La comparo con la primera
confusion_matrix
mosaic(confusion_matrix,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))




##PASO 4: ROC y Lift Charts
pred <- prediction(predictions= log_modelC$fitted.values, labels = log_modelC$model$Creditability)
perf <- performance(pred,"tpr","fpr") #La curva ROC

# Punto de corte óptimo del clasificador (probabilidad óptima)
cost.perf <- performance(pred, measure ="cost")
opt.cut   <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])] #La probabilidad óptima

#coordenadas de la probabilidad óptima sobre la ROC
x<-perf@x.values[[1]][which.min(cost.perf@y.values[[1]])]
y<-perf@y.values[[1]][which.min(cost.perf@y.values[[1]])]

#La ROC pintada con el punto
plot(perf,colorize=TRUE,type="l") 
abline(a=0,b=1)
points(x,y, pch=20, col="red")

# Área bajo la curva, para comparaciones entre modelos
AUC       <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values[[1]]


#Confussion con la P óptima de la ROC
predictions<-ifelse(test=log_modelC$fitted.values>opt.cut,yes=1, no=0)

confusion_matrixOpt<-table(log_modelC$model$Creditability,predictions,
                        dnn=c("observations", "predictions"))

confusion_matrixOpt
confusion_matrix

#plotting the confussion matrix
mosaic(confusion_matrixOpt,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))

#La primera
mosaic(confusion_matrix,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))




#liftcurve 
library(lift) #for lift curve
plotLift(log_modelC$fitted.values,log_modelC$model$Creditability, cumulative = TRUE, 
         n.buckets = 10)
TopDecileLift(log_modelC$fitted.values,log_modelC$model$Creditability)



#QUE ES ESTO???
# Plot the performance of the model applied to the evaluation set as an ROC curve.

pred <- prediction(predictions= log_modelC$fitted.values,
                   labels = log_modelC$model$Creditability)

# And then a lift chart
perf2 <- performance(pred,"lift","rpp")
plot(perf2, main="lift curve", colorize=T)
























