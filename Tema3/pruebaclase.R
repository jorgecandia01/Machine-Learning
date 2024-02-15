fdata <- read.table(file = "GITT+BA_auto.csv",
                    sep = ',',
                    header = TRUE,
                    na.strings = 'NA',
                    stringsAsFactors = FALSE)

library(tidyverse)      #data manipulation and visualization
library("car")  # For VIF
library("psych")  # For multi.hist
library("corrplot") #For corrplot
library("plot3D")
library("modelr")  # For add_predictions


cars <- fdata[,c(-9)]

#nas <- which(is.na(cars$mpg))
#cars <- cars[-nas,]
cars <- na.omit(cars)

summary(cars)


#DECLARO CADA VARIABLE
#AL FINAL USO EL data=
#mpg <- cars$mpg
#cylinders <- cars$cylinders
#displacement <- cars$displacement
#horsepower <- cars$horsepower
#weight <- cars$weight
#acceleration <- cars$acceleration
#year <- cars$model.year
#origin <- cars$origin


#SCATTERPLOT
pairs(~mpg+cylinders+displacement+horsepower+weight+acceleration+model.year+origin, 
      main='marketing scatterplots', col=c('blue'), data=cars)

corrplot.mixed(corr=cor(cars[, colnames(cars)], 
                        method="pearson"), tl.pos="lt", tl.srt=45, 
               addCoef.col = "black", data=cars) 


#CREO EL MODELO
model <- lm(mpg~cylinders+displacement+horsepower+weight+acceleration+model.year+origin, data=cars)
summary(model)
vif(model)
confint(model)

#MODELO ALTERNATIVO
model2 <- lm(mpg~cylinders*displacement+weight*horsepower+model.year+origin, data=cars)
summary(model2)
vif(model2)
confint(model2)


#VERIFICACIÓN
set.seed(101)
indexes <- sample(nrow(cars), 0.8*nrow(cars), replace = F)
trainData <- cars[indexes,]
testData <- cars[-indexes,]


#MODELO 1
model1Train <- lm(mpg~cylinders+displacement+horsepower+weight+acceleration+model.year+origin,
                  data=trainData)
summary(model1Train)
vif(model1Train)
confint(model1Train)

predictions = predict(model1Train, new=trainData)

res <- trainData$mpg - predictions
summary(res)
sd(res)

ggplot(data=model1Train, mapping= aes(x=model1Train$residuals))+
  geom_histogram(binwidth=0.5, col=c('blue'))

plot(model1Train, which=1,  id.n=NULL, col=c('blue'))

plot(model1Train, which=2,  id.n=3, col=c('blue'))

#PREDICCIONES MODELO 1

predicciontest1 = predict(model1Train, new=testData)

res_test1 <- testData$mpg - predicciontest1
summary(res_test1)
sd(res_test1)

hist(res_test1)

plot(model1Train, which=1,  id.n=NULL, col=c('blue'))

plot(model1Train, which=2,  id.n=3, col=c('blue'))

#MODELO 2
model2Train <- lm(mpg~cylinders*displacement+weight*horsepower+model.year+origin,
                  data=trainData)
summary(model2Train)
vif(model2Train)
confint(model2Train)

predictions2 = predict(model2Train, new=trainData)

res2 <- trainData$mpg - predictions2
summary(res2)
sd(res2)

ggplot(data=model2Train, mapping= aes(x=model2Train$residuals))+
  geom_histogram(binwidth=0.5, col=c('blue'))

plot(model2Train, which=1,  id.n=NULL, col=c('blue'))

plot(model2Train, which=2,  id.n=3, col=c('blue'))

#PREDICCIONES MODELO 2

predicciontest2 = predict(model2Train, new=testData)

res_test2 <- testData$mpg - predicciontest2
summary(res_test2)
sd(res_test2)

hist(res_test2)

plot(model2Train, which=1,  id.n=NULL, col=c('blue'))

plot(model2Train, which=2,  id.n=3, col=c('blue'))





















