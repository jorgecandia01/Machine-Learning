setwd('/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/P5')

library("psych")  # for multi-hist
library(aod)
library(ggplot2)
library(vcd) # For mosaic

data <- read.csv('Titanic.csv')
data <- data[-c(1,2,7)]
any(is.na(data))
data <- na.omit(data)
any(is.na(data))

summary(data)
str(data)
#data$Sex <- factor(data$Sex)
data$PClass <- factor(data$PClass)
data$Survived <- factor(data$Survived)



################## COMPLETE MODEL (ALL THE VARIABLES)

log_modelC <-glm(Survived~PClass+Age+Sex, family="binomial", data=data)
summary(log_modelC)

#Confidence Intervals for coefficients using profiled log-likelihood
confint(log_modelC)

#Confidence Intervals for coefficients using standarerrors
confint.default(log_modelC)


######Test of fit
#p value of degrees of freedom
with(log_modelC, pchisq(null.deviance -deviance, df.null-df.residual, lower.tail=FALSE))


#####Wald tests
wald.test(b=coef(log_modelC), Sigma=vcov(log_modelC), Terms=2:3)
wald.test(b=coef(log_modelC), Sigma=vcov(log_modelC), Terms=5)
wald.test(b=coef(log_modelC), Sigma=vcov(log_modelC), Terms=4)

# Testing the different between Pclass 2 and Pclass 3
# Creation of the vector needed 
vector<-cbind(0,1, -1, 0, 0)
wald.test(b=coef(log_modelC), Sigma=vcov(log_modelC), L=vector)
#LA CLASE TIENE MUCHO QUE VER
#En general todas las variables son muy significativas



################ PREDICTION

predicted <- data.frame(p_survived=log_modelC$fitted.values, survived=data$Survived)

#Oreno el dataframe en función de la probabilidad de sobrevivir (de - a +)
predicted <- predicted[
  order(predicted$p_survived, decreasing=FALSE),]

predictedVSsurvivded <- predicted
predicted$survived <- 1:nrow(predicted)



## Plotting the predicted probabilities
ggplot(data=predicted, aes(x=survived, y=p_survived)) +
  geom_point(aes(color=data$Sex), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of surviving")

ggplot(data=predicted, aes(x=survived, y=p_survived)) +
  geom_point(aes(color=log_modelC$Age), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of surviving")



#Confussion in the training set
predictions<-ifelse(test=log_modelC$fitted.values>0.5,yes=1, no=0)

confusion_matrix<-table(log_modelC$model$Survived,predictions,
                        dnn=c("observations", "predictions"))

confusion_matrix

#plotting the confussion matrix
mosaic(confusion_matrix,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))












################## MODEL 1 

log_model1 <-glm(Survived~Age+Sex+PClass+PClass*Sex, family="binomial", data=data)
summary(log_model1) ##AIC OF C IS 695.14

#Confidence Intervals for coefficients using profiled log-likelihood
confint(log_model1)

#Confidence Intervals for coefficients using standarerrors
confint.default(log_model1)


######Test of fit
#p value of degrees of freedom
with(log_model1, pchisq(null.deviance -deviance, df.null-df.residual, lower.tail=FALSE))


#####Wald tests
wald.test(b=coef(log_model1), Sigma=vcov(log_model1), Terms=1)
wald.test(b=coef(log_model1), Sigma=vcov(log_model1), Terms=2) 
#age no importa mucho, el AIC sin age solo sube 5 puntos (de 795 a 800)
wald.test(b=coef(log_model1), Sigma=vcov(log_model1), Terms=3)



################ PREDICTION

predicted1 <- data.frame(p_survived=log_model1$fitted.values, survived=data$Survived
                         , age=data$Age, sex=data$Sex)

#Oreno el dataframe en función de la probabilidad de sobrevivir (de - a +)
predicted1 <- predicted1[
  order(predicted1$p_survived, decreasing=FALSE),]

predictedVSsurvivded1 <- predicted1
predicted1$survived <- 1:nrow(predicted1)



## Plotting the predicted probabilities
ggplot(data=predicted1, aes(x=survived, y=p_survived)) +
  geom_point(aes(color=prueba$Sex), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of surviving")

ggplot(data=predicted1, aes(x=survived, y=p_survived)) +
  geom_point(aes(color=data$Survived), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of surviving")



#Confussion in the training set
predictions1<-ifelse(test=log_model1$fitted.values>0.5,yes=1, no=0)

confusion_matrix1<-table(log_model1$model$Survived,predictions1,
                        dnn=c("observations", "predictions"))

confusion_matrix1

##MODELC:   0,0 -> 372;   1,1 -> 222

#plotting the confussion matrix OF 1
mosaic(confusion_matrix1,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))



#plotting the confussion matrix OF C
mosaic(confusion_matrix,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))



##Probando log_model1 sin age, se averigua que el resultado es el mismo.
##El factor determinante para averiguar si un pasajero se salva o no, es el sexo.




