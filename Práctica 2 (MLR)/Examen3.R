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

wages <- wages[,-c(6)]

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

modelC <- lm(wage ~ year+age+maritl+race+education+jobclass+health+health_ins, data=wages)
summary(modelC)
vif(modelC)
confint(modelC)
  
model <- lm(wage ~ year+age*maritl+race*education+jobclass+health*health_ins, data=wages)
summary(model)
vif(model)
confint(model)





##############PASO 2
modelC <- lm(wage ~ year+age+maritl+race+education+jobclass+health+health_ins, data=wages)
summary(modelC)
vif(modelC)
confint(modelC)

model <- lm(wage ~ year*age*maritl*race*health*education*health_ins*jobclass+health, data=wages)
summary(model)
vif(model)
confint(model)


##########TEST
modelTrain <- lm(wage~year*age*maritl*race*health*education*health_ins*jobclass+health, data=wages)
summary(modelTrain)

predictionsTrain = predict(modelTrain, new=trainData)
predictionsTest = predict(modelTrain, new=testData)

resTrain <- trainData$wage - predictionsTrain
resTest  <- testData$wage - predictionsTest

summary(resTrain)
summary(resTest)
sd(resTrain)
sd(resTest)

#RSE %
RSE <- 34.17/mean(wages$wage) * 100  # 29.96%


summary(wages$wage) 
sd(wages$wage) #La desviacion de los wages reales

ggplot(data=modelTrain, mapping= aes(x=modelTrain$residuals))+
  geom_histogram(binwidth=0.5, col=c('blue'))

hist(resTest)

plot(modelTrain, which=1,  id.n=NULL, col=c('blue'))

plot(modelTrain, which=2,  id.n=3, col=c('blue'))

plot(modelTrain, which=4,  id.n=3, col=c('blue'))
######################################


#PASO 3

set.seed(100)
indexes <- sample(nrow(wages), 0.8*nrow(wages), replace = F)
trainData <- wages[indexes,]
testData <- wages[-indexes,]

#MODELO COMPLETO (modelC)

modelCTrain <- lm(wage ~ year+age+maritl+race+education+jobclass+health+health_ins, data=trainData)
summary(modelCTrain)

predictionsCTrain = predict(modelCTrain, new=trainData)
predictionsCTest = predict(modelCTrain, new=testData)

resCTrain <- trainData$wage - predictionsCTrain
resCTest  <- testData$wage - predictionsCTest

summary(resCTrain)
summary(resCTest)
sd(resCTrain)
sd(resCTest)


ggplot(data=modelCTrain, mapping= aes(x=modelCTrain$residuals))+
  geom_histogram(binwidth=0.5, col=c('blue'))

hist(resCTest)

sd(wages$wage) #La desviacion de los wages reales

plot(modelCTrain, which=1,  id.n=NULL, col=c('blue'))

plot(modelCTrain, which=2,  id.n=3, col=c('blue'))

plot(modelCTrain, which=4,  id.n=3, col=c('blue'))




#MODELO REFINADO (model)

modelTrain <- lm(wage ~ year+age*maritl+race*education+jobclass+health*health_ins, data=trainData)
summary(modelTrain)

predictionsTrain = predict(modelTrain, new=trainData)
predictionsTest = predict(modelTrain, new=testData)

resTrain <- trainData$wage - predictionsTrain
resTest  <- testData$wage - predictionsTest

summary(resTrain)
summary(resTest)
sd(resTrain)
sd(resTest)

#RSE %
RSE <- 33.47/mean(wages$wage) * 100  # 29.96%


summary(wages$wage) 
sd(wages$wage) #La desviacion de los wages reales

ggplot(data=modelTrain, mapping= aes(x=modelTrain$residuals))+
  geom_histogram(binwidth=0.5, col=c('blue'))

hist(resTest)

plot(modelTrain, which=1,  id.n=NULL, col=c('blue'))

plot(modelTrain, which=2,  id.n=3, col=c('blue'))

plot(modelTrain, which=4,  id.n=3, col=c('blue'))



















