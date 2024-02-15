setwd("/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/Práctica 6 (Decision Trees + Ensembles)")

library("corrplot")     #For corrplot
library(tidyverse)      #data manipulation and visualization
library(C50)
library(class)          # to call class package for kNN
library(caret)          #Las matrices de confusión pero más fancy

library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(DMwR2)       # for unscale

raw_data <- read.table("page-blocks.data")

#Doy los nombres a las variables, que inicialmente no los tienen
data <- data.frame(height = raw_data$V1) #declaro el dataframe con V1
data$lenght <- raw_data$V2
data$area <- raw_data$V3
data$eccen <- raw_data$V4
data$p_black <- raw_data$V5
data$p_and <- raw_data$V6
data$mean_tr <- raw_data$V7
data$blackpix <- raw_data$V8
data$blackand <- raw_data$V9
data$wb_trans <- raw_data$V10
data$group <- raw_data$V11




##### PREPROCESADO Y EXPLORACIÓN DE LOS DATOS #####
data <- na.omit(data)
data$group <- factor(data$group) #Los DT necesitan factors de variable dependiente para classification

str(data)
summary(data)

corrplot_selection <- colnames(raw_data)
corrplot.mixed(corr=cor(raw_data[, 1:11],method="pearson"), 
               tl.pos="lt", tl.srt=5, addCoef.col = "black")


#SEPARO TRAIN Y TEST
set.seed(123)
#Escojo sólo estas variables (es lo que se ha considerado oportuno) y las normalizo
#subdata <- as.data.frame(lapply(data[,c(1,4,5,8)], normalize)) 
#subdata <- normalizeData(data[,c(1,4,5,8)])
index <- sample(c(TRUE, FALSE), nrow(data), prob=c(0.7, 0.3), replace=TRUE)
train <- data[index,]
test <- data[!index,]
#La variable a predecir (target)
train_labels <- data[index,11]
test_labels  <- data[!index,11]



##### 1.- C4.5 CON C5.0 #####
#CREACIÓN DEL ÁRBOL
arbol <- C5.0(group ~ ., data=train,
                    control = C5.0Control(
                        noGlobalPruning = FALSE, # Pruning is in effect
                        CF= 0.25))  #Higher CF less prunning

summary(arbol) #Precisión del 98.1%

#REPRESENTACIÓN GRÁFICA
plot(arbol, trial=0, subtree=NULL)

#RESULTADOS
#Predicción train
pred_train <- predict(arbol, newdata = train, type ="class")
caret::confusionMatrix(pred_train, train$group) # Precisión del 98.13%

#Predicción test
predictions <- predict(arbol, newdata = test, type ="class")
caret::confusionMatrix(predictions, test$group) #Precisión del 97.37%


#REGLAS DEL CONOCIMIENTO
ruleModel <- C5.0(group  ~ ., data=train,rules = TRUE,
                    control = C5.0Control(
                      noGlobalPruning = FALSE, # Pruning is in effect
                      CF= 0.25))  #Higher CF less prunning

summary(ruleModel)




##### 2.- CART CON RPART #####
#CREACIÓN DEL ÁRBOL
arbol <- rpart(formula= group ~ ., data=train, method='class')

#RESULTADO
print(arbol)
summary(arbol)

#REPRESENTACIÓN GRÁFICA
#Fitting the plotting allowing labels in several lines
split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width = 10), collapse = "\n")
  }
  labs
}

#El Plot
rpart.plot(arbol, type=1, branch=0,tweak=2.3, 
           fallen.leaves = TRUE,
           varlen = 0, faclen = 0, split.fun=split.fun)

##Otra opción
plot(arbol, uniform=TRUE, compress=TRUE)
text(arbol, use.n=TRUE)


#BUSCO EL MEJOR CP (PARA MÍNIMO ERROR)
printcp(arbol, digits=6)
#El mejor
best_cp<- arbol$cptable[which.min(arbol$cptable[,"xerror"]),"CP"]

#Error del DT para distintos valores de cp
plotcp(arbol, lty=2 , col="red", upper="size" )
plotcp(arbol, lty=2 , col="red", upper="splits" )

#PODO EL ÁRBOL CON EL MEJOR CP
arbol_podado <- prune(arbol, cp=best_cp)
print(arbol_podado)
summary(arbol_podado)
#Plot del árbol podado
rpart.plot(arbol_podado, type=1, branch=0,tweak=1.8, 
           fallen.leaves = TRUE,
           varlen = 0, faclen = 0, split.fun=split.fun)


#RESULTADOS
#Predicción train
pred_train <- predict(arbol_podado, newdata = train, type ="class")
caret::confusionMatrix(pred_train, train$group) # Precisión del 97.14%
#Comparación con el arbol sin podar
pred_train <- predict(arbol, newdata = train, type ="class")
caret::confusionMatrix(pred_train, train$group) # Precisión del 97.37%
#El árbol sin podar tiene un poco más de precisión pero es más sencillo (tiene menos nodos)

#Predicción test
predicciones <- predict(arbol_podado, newdata = test, type ="class")
caret::confusionMatrix(predicciones, test$group) # Precisión del 96.69%
#Comparación con el arbol sin podar
predicciones <- predict(arbol, newdata = test, type ="class")
confusionMatrix(predicciones, test$group) # Precisión del 96.75%
#A pesar de ser un DT más sencillo, el arbol podado tiene ligeramente menos precisión (0.06%)



#REGLAS DEL CONOCIMIENTO
rpart.rules(arbol, style = "tall", cover=TRUE,
            nn=TRUE, clip.facs = TRUE)

rpart.rules(arbol_podado, style = "tall", cover=TRUE,
            nn=TRUE, clip.facs = TRUE)




























