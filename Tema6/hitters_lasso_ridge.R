setwd("/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/Tema6")

library(glmnet)
library(tidyverse)

#library(ISLR)
#data(Hitters, package = "ISLR")

#loading data
Hitters <- read_csv("Hitters.csv")

#detecting missing values
sum(is.na(Hitters))
sum(is.na(Hitters$Salary)) #missing values in Salary

#removing missing values
Hitters = na.omit(Hitters)
sum(is.na(Hitters))

#Removing first column with names
Hitters<-Hitters[, -1]
names(Hitters)




### PASO 1 COMÚN A LOS 2 MODELOS

# glmnet does not allow formula. Preparation is required
#1.- CREO UNA MATRIZ CON LOS DATOS Y UNA LISTA CON LA VARIABLE EXPLICADA PORQUE LO REQUIERE GLMNET
X = model.matrix(Salary ~ ., Hitters)[, -1] #El -1 quita el intercept que mete model.matrix
y = Hitters$Salary

# Simple regression model for comparison
#1.2.-CREO UN MODELO SIN REG PARA COMPARAR
fit = lm(Salary ~ ., Hitters)

summary(fit)

coef(fit)

sum(abs(coef(fit)[-1]))

sum(coef(fit)[-1] ^ 2)





############### RIDGE MODEL
par(mfrow = c(1, 2)) #plottear las gráficas apilandolas en 2 columnas de 1 fila
#2.- CREO EL MODELO CON REGULARIZACIÓN RIDGE (alpha = 0)
fit_ridge = glmnet(X, y, alpha = 0) #alpha = 0 -> Ridge

#2.2.- PLOTS DE PENALIZACIONES DE COEFS PARA DISTITAS LAMBDAS (ninguno es cero)
plot(fit_ridge, label = T)
plot(fit_ridge, xvar = "lambda", label = TRUE)


#3.- CON CV.GLMNET CREO UN MODELO CON CV DE K=10 (DEFAULT) DEL QUE PODEMOS PEDIR LA LAMBDA ÓPTIMA (MIN Y 1-SE)
par(mfrow = c(1, 1)) #plottear las gráficas apilandolas en 1 columna de 1 fila (lo normal)
fit_ridge_cv = cv.glmnet(X, y, alpha = 0) 
plot(fit_ridge_cv)


fit_ridge_cv$lambda.min #####3.2.- Best Lambda MIN #####
fit_ridge_cv$lambda.1se #####3.3.- Best Lambda with 1-SE #####


# fitted coefficients, , using 1-SE rule lambda, default behavior
#1-SE rule is defined as selecting the most parsimonious model whose 
#error is no more than one standard error above the error of the best 
#model. The main point of the 1 SE rule is to choose the simplest model
#whose accuracy is comparable with the best model.

coef(fit_ridge_cv) #Por defecto lambda = 1se (?)

# fitted coefficients, using minimum lambda
coef(fit_ridge_cv, s = "lambda.min")

# penalty term using minimum lambda
sum(coef(fit_ridge_cv, s = "lambda.min")[-1] ^ 2)

# fitted coefficients, using 1-SE rule lambda
coef(fit_ridge_cv, s = "lambda.1se")

#4.- PREDICCIONES CON LOS 2 LAMBDAS, DE LOS PROPIOS DATOS
# predict using minimum lambda
predict(fit_ridge_cv, X, s = "lambda.min")

# predict using 1-SE rule lambda, default behavior
predict(fit_ridge_cv, X)

#4.2.- ERROR DE LAS PREDICCIONES, LA MEDIA CUADRADA (MSE)
# calcualte "train error"
mean((y - predict(fit_ridge_cv, X)) ^ 2)

# CV-RMSEs
sqrt(fit_ridge_cv$cvm)

# CV-RMSE using minimum lambda
sqrt(fit_ridge_cv$cvm[fit_ridge_cv$lambda == fit_ridge_cv$lambda.min])

# CV-RMSE using 1-SE rule lambda ####MAS O MENOS LO MISMO Q √(4.2.-)
##pero no es eso no??
sqrt(fit_ridge_cv$cvm[fit_ridge_cv$lambda == fit_ridge_cv$lambda.1se]) 






############### LASSO MODEL
par(mfrow = c(1, 2))
#2.- CREO EL MODELO CON REGULARIZACIÓN LASSO (alpha = 1)
fit_lasso = glmnet(X, y, alpha = 1) #alpha = 1 -> Lasso

#plot for  illustrating how much the coefficients are penalized  
#for different values of  lamdbda. 
# Notice some of the coefficients are forced to be zero
#2.2.- PLOTS DE PENALIZACIONES DE COEFS PARA DISTITAS LAMBDAS
plot(fit_lasso)
plot(fit_lasso, xvar = "lambda", label = TRUE)

# selecting the best lambda using CV
#3.- CON CV.GLMNET CREO UN MODELO CON CV DE K=10 (DEFAULT) DEL QUE PODEMOS PEDIR
#LA LAMBDA ÓPTIMA MIN Y 1-SE
par(mfrow = c(1, 1))
fit_lasso_cv = cv.glmnet(X, y, alpha = 1)
plot(fit_lasso_cv)

# fitted coefficients, using 1-SE rule lambda, default behavior
coef(fit_lasso_cv) #####3.3.- coeficientes con Lambda 1-SE #####

# fitted coefficients, using minimum lambda
coef(fit_lasso_cv, s = "lambda.min") #####3.2.- coeficientes con Lambda MIN #####

# penalty term using minimum lambda
sum(coef(fit_lasso_cv, s = "lambda.min")[-1] ^ 2)

# fitted coefficients, using 1-SE rule lambda ##Lo mismo q 3.3 
coef(fit_lasso_cv, s = "lambda.1se")

# penalty term using 1-SE rule lambda
sum(coef(fit_lasso_cv, s = "lambda.1se")[-1] ^ 2)

#4.- PREDICCIONES CON LAS 2 LAMBDAS, DE LOS PROPIOS DATOS
# predict using minimum lambda
predict(fit_lasso_cv, X, s = "lambda.min")

# predict using 1-SE rule lambda, default behavior
predict(fit_lasso_cv, X)

#4.2.- ERROR DE LAS PREDICCIONES
# calcualte "train error"
mean((y - predict(fit_lasso_cv, X)) ^ 2)


# CV-RMSEs
sqrt(fit_lasso_cv$cvm)


# CV-RMSE using minimum lambda
sqrt(fit_lasso_cv$cvm[fit_lasso_cv$lambda == fit_lasso_cv$lambda.min])

# CV-RMSE using 1-SE rule lambda 
sqrt(fit_lasso_cv$cvm[fit_lasso_cv$lambda == fit_lasso_cv$lambda.1se]) 





