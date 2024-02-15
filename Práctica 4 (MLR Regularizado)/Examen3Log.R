#PASO 0
setwd('/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/E3')
#install.packages('ISLR')

library(ISLR)
library(tidyverse)      #data manipulation and visualization
library("car")  # For VIF
library("psych")  # For multi.hist
library("corrplot") #For corrplot
library("modelr")  # For add_predictions
library(varhandle)

#data(package = 'ISLR') #Inspecciono datasets de ISLR
wages <- as.data.frame(Wage)


#PASO 1
any(is.na(wages)) #No hay NAs en el dataframe

summary(wages)
str(wages)

wages <- wages[,-c(6)] #Sólo hay una región

pairs(~year+age+maritl+race+education+jobclass+health+health_ins+logwage+wage, 
      main='scatterplots', col=c('blue'), data=wages)

pairs(~wage+maritl+race+education+jobclass, #Realmente no se puede apreciar nada
      main='scatterplots', col=c('blue'), data=wages)

wagesPrueba <- wages[,c(1,2,5,10)]

library(ggcorrplot)
model.matrix(~0+., data=wagesPrueba) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)



#PASO 2

modelC <- lm(logwage ~ year+age+maritl+race+education+jobclass+health+health_ins, data=wages)
summary(modelC)
vif(modelC)
confint(modelC)

model <- lm(logwage ~ year+age*maritl+race*education+jobclass+health*health_ins, data=wages)
summary(model)
vif(model)
confint(model)



#PASO 3

set.seed(100)
indexes <- sample(nrow(wages), 0.8*nrow(wages), replace = F)
trainData <- wages[indexes,]
testData <- wages[-indexes,]

#MODELO COMPLETO (modelC)

modelCTrain <- lm(logwage ~ year+age+maritl+race+education+jobclass+health+health_ins, data=trainData)
summary(modelCTrain)

predictionsCTrain = predict(modelCTrain, new=trainData)
predictionsCTest = predict(modelCTrain, new=testData)

resCTrain <- trainData$logwage - predictionsCTrain
resCTest  <- testData$logwage - predictionsCTest

summary(resCTrain)
summary(resCTest)
sd(resCTrain)
sd(resCTest)


ggplot(data=modelCTrain, mapping= aes(x=modelCTrain$residuals))+
  geom_histogram(binwidth=0.01, col=c('blue'))

hist(resCTest)

sd(wages$logwage) #La desviacion de los wages reales

plot(modelCTrain, which=1,  id.n=NULL, col=c('blue'))

plot(modelCTrain, which=2,  id.n=3, col=c('blue'))

plot(modelCTrain, which=4,  id.n=3, col=c('blue'))



#MODELO REFINADO (model)

modelTrain <- lm(logwage ~ year+age*maritl+race*education+jobclass+health*health_ins, data=trainData)
summary(modelTrain)

#predictionsTrain = predict(modelTrain, new=trainData)
predictionsTest = predict(modelTrain, new=testData)

#resTrain <- trainData$logwage - predictionsTrain
resTest  <- testData$logwage - predictionsTest

summary(resTrain)
summary(resTest)
sd(resTrain)
sd(resTest)

#RSE %, % RMSE
RSE <- 0.2749/mean(wages$logwage) * 100  # 5.90% Por que un R^2 tan malo con RSE tan bueno?
                                      #Quizá por tanta variable cualitativa no significativa?
                                    #Es esto una muestra de que la R^2 no lo es todo?
#Tengo un RSE tan bajo porque al ser escala log, el RSE baja igual que lo han hecho los wages?

#TENGO QUE DESHACER EL LOGARITMO DESPUES DE HACER LAS PREDICCIONES
          
summary(wages$logwage) 
sd(wages$logwage) #La desviacion de los wages reales

ggplot(data=modelTrain, mapping= aes(x=modelTrain$residuals))+
  geom_histogram(binwidth=0.5, col=c('blue'))

hist(resTest)

plot(modelTrain, which=1,  id.n=NULL, col=c('blue'))

plot(modelTrain, which=2,  id.n=3, col=c('blue'))

plot(modelTrain, which=4,  id.n=3, col=c('blue'))

















