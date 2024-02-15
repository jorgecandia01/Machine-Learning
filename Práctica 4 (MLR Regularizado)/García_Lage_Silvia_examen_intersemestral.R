#############################################################################################
##################################Examen Intersemestral 2020#################################
#############################################################################################

install.packages("ISLR")
install.packages("glmnet")

library(ISLR) # for obtaining data
library("caret")    # esto es para el createDataPartition
library("car")  # For VIF
library(glmnet)

data(Carseats) 
View(Carseats)

Carseats$ShelveLoc <- as.factor(Carseats$ShelveLoc)
Carseats$Urban <- as.factor(Carseats$Urban)
Carseats$US <- as.factor(Carseats$US)

set.seed(123)
sample <- sample(c(TRUE,FALSE),nrow(Carseats),replace=T,prob=c(0.8,0.2))
train <- Carseats[sample,]
test <- Carseats[!sample,]

carsdata = na.omit(train)
all.empty = rowSums(is.na(carsdata))==ncol(carsdata)
cars.clean = carsdata[!all.empty,]

##############################################################################################
# 1. Obtener un modelo de regresi?n lineal multivariable capaz de predecir las ventas (Sales)#
# en funci?n de todas las variables disponibles y sin normalizar. ?Qu? valor de Adjusted     #
# Rsquared tiene el modelo encontrado?                                                       #
##############################################################################################

model <- lm(Sales~.,data=Carseats)
summary(model)

##############################################################################################
# 2. En el modelo obtenido en la pregunta 1 ?cu?l o cu?les de las siguientes variables       #
# influyen negativamente en la venta de sillas de ni?o para coche?                           #
##############################################################################################

confint(model)
vif(model)

# Price, Age, Education y USYes

##############################################################################################
# 3. En el modelo obtenido en la pregunta 1 de las siguientes variables ?hay alguna de       #
# la que se podr?a prescindir por ser poco significativa?                                    #
##############################################################################################

# population, education, UrbanYes y USYes
  
##############################################################################################
# 4. En el modelo obtenido en la pregunta 1 ?cu?l es el valor de SCR (Suma de Cuadrados de   #
# los Residuos)?                                                                             #
##############################################################################################

train_SCR_MLR <- 1.019*1.019*388
train_SCR_MLR
sum(resid(model)^2)

##############################################################################################
# 5. Usando el conjunto de datos descrito en la pregunta 1 obtener un modelo de regresi?n    #
# lineal tipo RIDGE capaz de predecir las ventas (Sales) en funci?n de todas las variables   #
# disponibles y sin normalizar.?Qu? valor de lambda ser?a m?s conveniente emplear?           #
##############################################################################################

X = model.matrix(Sales~.,data=train)[,-1]
y = train$Sales
par(mfrow=c(1,2))
fit_ridge = glmnet(X,y,alpha=0)
par(mfrow=c(1,1))
fit_ridge_cv = cv.glmnet(X,y,alpha=0)
fit_ridge_cv$lambda.min

##############################################################################################
# 6. Con el modelo RIDGE obtenido en la pregunta 5 ?cu?l es el valor medio de los errores de #
# predicci?n con el conjunto de test?                                                        #
##############################################################################################

x_test <- model.matrix(Sales~.,data=test)[,-1]
y_test = test$Sales
prediccionRidgeMinLambda <- predict(fit_ridge_cv,newx=x_test,s="lambda.min")
mean(y_test-prediccionRidgeMinLambda)
sqrt(fit_ridge_cv$cvm[fit_ridge_cv$lambda==fit_ridge_cv$lambda.min])

##############################################################################################
# 7. Usando el conjunto de datos descrito en la pregunta 1 obtener un modelo de regresi?n    #
# lineal tipo LASSO capaz de predecir las ventas (Sales) en funci?n de todas las variables   #
# disponibles y sin normalizar.?Qu? valor de lambda ser?a m?s conveniente emplear?         #
##############################################################################################

X_Lasso = model.matrix(Sales~.,data=train)[,-1]
y_Lasso = train$Sales
par(mfrow=c(1,2))
fit_ridge = glmnet(X,y,alpha=1)
par(mfrow=c(1,1))
fit_ridge_cv = cv.glmnet(X,y,alpha=1)
fit_ridge_cv$lambda.min

#############################################################################################
# 8. Con el modelo LASSO obtenido en la pregunta 7 ?cu?l es el valor medio de los errores   #
# de predicci?n con el conjunto de test?                                                    #
#############################################################################################

x_test_Lasso <- model.matrix(Sales~.,data=test)[,-1]
y_test_Lasso = test$Sales
mean(y_test-prediccionRidgeMinLambda)
sqrt(fit_ridge_cv$cvm[fit_ridge_cv$lambda==fit_ridge_cv$lambda.min])

#############################################################################################
# 9. Usando el conjunto de datos descrito en la pregunta 1 obtener un modelo de regresi?n   #  
# lineal tipo ELASTIC NET capaz de predecir las ventas (Sales) en funci?n de todas las      #
# variables disponibles y sin normalizar. ?El valor de alpha ?? sugiere un modelo:           #
#############################################################################################

glmnet_grid <- expand.grid(alpha = c(0, .1, .2, .4, .6, .8, 1),lambda = seq(.01, .2, length = 20))
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_fit <- train(Sales~.,data=train,method="glmnet",preProcess=c("center","scale"),
                    tuneGrid=glmnet_grid,trControl=glmnet_ctrl)
glmnet_fit$bestTune

#############################################################################################
# 10. Con el modelo ELASTIC NET obtenido en la pregunta 9 ?cu?l es el valor de SCR (Suma de #
# Cuadrados de los Residuos) con el conjunto de entrenamiento?                              #
#############################################################################################

glmnet_grid_best <- expand.grid(alpha = 0.8,lambda = 0.2)
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_best <- train(Sales ~ ., data = train,method = "glmnet",preProcess = c("center", "scale"),
                     tuneGrid = glmnet_grid_best,trControl = glmnet_ctrl)
sum(resid(glmnet_best)^2)

#############################################################################################
# 11. ?El modelo Lasso sugiere reducir las variables del modelo?                            #
#############################################################################################

# NO LO ENTIENDO --> S? el modelo Lasso sugiere reducir las variables entre 7 y 9   