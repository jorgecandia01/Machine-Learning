setwd("/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/Práctica 4 (MLR Regularizado)")

##### PREPROCESADO DE LOS DATOS #####
library(ISLR)
library(tidyverse)      #data manipulation and visualization
library("car")  # For VIF
library("psych")  # For multi.hist
library("corrplot") #For corrplot
library("modelr")  # For add_predictions
library(varhandle)

library(RCurl)   # for using getURL
library(prettyR) # for nice printing
library(caret)   # for model regression
library(glmnet)  # for lasso regression

#data(package = 'ISLR') #Inspecciono datasets de ISLR e importo Wage
wages <- as.data.frame(Wage)


#Preprocesado. Todas las variables discretas vienen ya en factor
any(is.na(wages)) #No hay NAs en el dataframe
summary(wages)
str(wages)
wages <- wages[,-c(6)] #Sólo hay una región


#Breve observación de los datos
library(ggcorrplot)
model.matrix(~0+., data=wages[,c(1,2,5,10)]) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


#Partición de los datos para training y test
set.seed(100)
indexes <- sample(nrow(wages), 0.8*nrow(wages), replace = F)
training <- wages[indexes,]
test <- wages[-indexes,]
rm(indexes) #borro el index para tener el environment más limpio



#### CASO 0 --> RESULTADOS CON MODELO BASE #### (Se usará para comparar)
base_model <- lm(logwage ~ year+age*maritl+race*education+jobclass+health*health_ins, data=wages)
summary(base_model)
vif(base_model)
confint(base_model)

#PREDICCIONES // RESULTADOS
predictionsTest <- predict(base_model, new=test)
residualsTest  <- test$logwage - predictionsTest

summary(residualsTest)
sd(residualsTest)

# RMSE
RMSE <- sqrt(mean(residualsTest^2)) #0.2893
# % RMSE: 
PRMSE <- RMSE/mean(test$logwage) * 100 #5.907% 


# predictionsTraining <- predict(base_model, new=training)
# residualsTraining  <- training$logwage - predictionsTraining
# sqrt(mean(residualsTraining^2)) #0.2734
# sqrt(mean(residualsTraining^2))/mean(training$logwage) * 100 #5.88%




#PASO COMÚN A RIDGE Y LASSO
# glmnet does not allow formula. Preparation is required
#1.- CREO UNA MATRIZ CON LOS DATOS Y UNA LISTA CON LA VARIABLE EXPLICADA PORQUE LO REQUIERE GLMNET
X = model.matrix(logwage ~ year+age*maritl+race*education+jobclass+health*health_ins, training)[, -1] 
#El -1 quita el intercept que mete model.matrix
y = training$logwage

Xtest = model.matrix(logwage ~ year+age*maritl+race*education+jobclass+health*health_ins, test)[, -1] 
ytest = test$logwage




##### CASO 1 -> REGULARIZACIÓN RIDGE #####

#2.- CREO EL MODELO CON REGULARIZACIÓN RIDGE (alpha = 0)
par(mfrow = c(1, 2)) #plottear las gráficas apilandolas en 2 columnas de 1 fila
fit_ridge = glmnet(X, y, alpha = 0) #alpha = 0 -> Ridge

#2.2.- PLOTS DE PENALIZACIONES DE COEFS PARA DISTITAS LAMBDAS (ninguno es cero)
plot(fit_ridge, label = T)
plot(fit_ridge, xvar = "lambda", label = TRUE)
#Hay tantas variables por los factores y los productos


#3.- CON CV.GLMNET CREO UN MODELO CON CV DE K=10 (DEFAULT) DEL QUE PODEMOS PEDIR LA LAMBDA ÓPTIMA (MIN Y 1-SE)
par(mfrow = c(1, 1)) #plottear las gráficas apilandolas en 1 columna de 1 fila (lo normal)
fit_ridge_cv = cv.glmnet(X, y, alpha = 0) 
plot(fit_ridge_cv)

fit_ridge_cv$lambda.min #####3.2.- Best Lambda MIN #####
fit_ridge_cv$lambda.1se #####3.3.- Best Lambda with 1-SE #####

# fitted coefficients, using minimum lambda
coef(fit_ridge_cv, s = "lambda.min") #ESTO ES EL VALOR DEL COEFICIENTE, LO QUE SE LE SUMA O LO QUE SE LE MULTIPLICA?
# penalty term using minimum lambda
sum(coef(fit_ridge_cv, s = "lambda.min")[-1] ^ 2)
# fitted coefficients, using 1-SE rule lambda
coef(fit_ridge_cv, s = "lambda.1se")


#4.- PREDICCIONES CON LOS 2 LAMBDAS
# predict using minimum lambda
predictionsTest <-predict(fit_ridge_cv, Xtest, s = "lambda.min")
# predict using 1-SE rule lambda, default behavior
predictionsTest <- predict(fit_ridge_cv, Xtest)

#4.2.- ERROR DE LAS PREDICCIONES (elegir arriba si hacerlo con 1se ó min)
residualsTest <- ytest - predictionsTest

summary(residualsTest)
sd(residualsTest)

# RMSE
RMSE <- sqrt(mean(residualsTest^2)) #0.2949 min // 0.3030 1se
# % RMSE: 
PRMSE <- RMSE/mean(test$logwage) * 100 #6.300% min //6.473% 1se





##### CASO 2 -> REGULARIZACIÓN LASSO #####

#2.- CREO EL MODELO CON REGULARIZACIÓN LASSO (alpha = 1)
par(mfrow = c(1, 2)) #plottear las gráficas apilandolas en 2 columnas de 1 fila
fit_lasso = glmnet(X, y, alpha = 1) #alpha = 0 -> Lasso

#2.2.- PLOTS DE PENALIZACIONES DE COEFS PARA DISTITAS LAMBDAS 
plot(fit_lasso, label = T)
plot(fit_lasso, xvar = "lambda", label = TRUE)
#Hay tantas variables por los factores y los productos


#3.- CON CV.GLMNET CREO UN MODELO CON CV DE K=10 (DEFAULT) DEL QUE PODEMOS PEDIR LA LAMBDA ÓPTIMA (MIN Y 1-SE)
par(mfrow = c(1, 1))
fit_lasso_cv = cv.glmnet(X, y, alpha = 1)
plot(fit_lasso_cv)

fit_lasso_cv$lambda.min #####3.2.- Best Lambda MIN #####
fit_lasso_cv$lambda.1se #####3.3.- Best Lambda with 1-SE #####

# fitted coefficients, using minimum lambda
coef(fit_lasso_cv, s = "lambda.min") #ESTO ES EL VALOR DEL COEFICIENTE, LO QUE SE LE SUMA O LO QUE SE LE MULTIPLICA?
# penalty term using minimum lambda
sum(coef(fit_lasso_cv, s = "lambda.min")[-1] ^ 2)
# fitted coefficients, using 1-SE rule lambda
coef(fit_lasso_cv, s = "lambda.1se")


#4.- PREDICCIONES CON LOS 2 LAMBDAS
# predict using minimum lambda
predictionsTest <-predict(fit_lasso_cv, Xtest, s = "lambda.min")
# predict using 1-SE rule lambda, default behavior
predictionsTest <- predict(fit_lasso_cv, Xtest)

#4.2.- ERROR DE LAS PREDICCIONES (elegir arriba si hacerlo con 1se ó min)
residualsTest <- ytest - predictionsTest

summary(residualsTest)
sd(residualsTest)

# RMSE
RMSE <- sqrt(mean(residualsTest^2)) #0.3005 min // 0.2936 1se
# % RMSE: 
PRMSE <- RMSE/mean(test$logwage) * 100 #6.419% min //6.272% 1se








##### CASO 3 -> REGULARIZACIÓN MIXTA #####
# Parameters for regularization
# Lambda  is the regularization parameter. Several values are tested
# alpha= 0 is RIDGE, alpha=1 is LASSO. Other intermediate values are tested
#This variation of alpha is called mixing percentage
#CREO UN GRID CON LOS VALORES DE ALPHA Y LAMBDA QUE VOY A PROBAR
glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))

# CV method using 10 folds
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_fit <- train(logwage ~ year+age*maritl+race*education+jobclass+health*health_ins, data = training,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl)

# Results obtained -> R^2 y RMSE para cada alpha y lambda
glmnet_fit

# best result -> Te elige automáticamente el mejor modelo
glmnet_fit$bestTune #MEJOR RESULTADO: alpha = 0.1, lambda = 0.01, modelo n21

# plotting results
trellis.par.set(caretTheme())
plot(glmnet_fit, scales = list(x = list(log = 2)))


#PREDICCIONES:
#predict es una función genérica de tipo R3 en R que sirve para cualquier objeto y está 
#preparada para coger siempre la mejor opción del objeto que se pase, pero aún así se
#rehará el CV para los alpha y lambda óptimos:

# Rebuilding the model with best lamda value identified
glmnet_grid_best <- expand.grid(alpha = 0.1,lambda = 0.01)

# CV method using 10 folds
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_best <- train(logwage ~ year+age*maritl+race*education+jobclass+health*health_ins, data = training,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneGrid = glmnet_grid_best,
                     trControl = glmnet_ctrl)

# Ideas for Obtaining the coeeficients of the model 
coef(glmnet_best) #NULL???
glmnet_best$results

#Predicciones con datos nuevos
predictionsTest <- predict(glmnet_best, newdata = test) 

#TENGO QUE HACER ESTO?????
#Paso las predicciones a escala lineal para hacer las comparaciones con wage
#predictionsTest <- exp(predictionsTest)

#Residuos de las predicciones de test, de vuelta a log para ser coherente con el resto de datos
#residualsTest  <- log(abs(test$wage - predictionsTest)) 

residualsTest  <- test$logwage - predictionsTest

summary(residualsTest)
sd(residualsTest)

# RMSE
RMSE <- sqrt(mean(residualsTest^2)) #0.2949
# % RMSE: 
PRMSE <- RMSE/mean(test$logwage) * 100 #6.2987%































