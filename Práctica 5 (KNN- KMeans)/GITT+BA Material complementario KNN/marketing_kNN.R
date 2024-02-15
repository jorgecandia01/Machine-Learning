#'Marketing Data Set
#'
#'@description A data frame containing the impact of three advertising medias (youtube, facebook and
#'  newspaper) on sales. Data are the advertising budget in thousands
#'  of dollars along with the sales. The advertising experiment has been repeated 200 times.
#'@name marketing
#'@docType data
#'@usage data("marketing")
#'@format A data frame with 200 rows and 4 columns.
#' @examples
#' data(marketing)
#' res.lm <- lm(sales ~ youtube*facebook, data = marketing)
#' summary(res.lm)

## From the CONSOLE execute the following command
#devtools::install_github("kassambara/datarium/tree/master/data")

#  Packages
library(tidyverse)      #data manipulation and visualization
library(class)          # to call class package for kNN
library(caret)          # for building the model

#Loading marketing data
load("marketing.rda")

summary(marketing)

# Normalization is convenient if the variables have different
# ranges. In this case they have similar ranges but the way
# the way for normalization is presented

#Data normalistion 
# creating a normalize function for easy conversion.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) } 

 # lapply creates list (reason why it is converted to dataframe) and 
#it applies the defined function (which is 'normalize') to all 
#the list values 

marketing.n<- as.data.frame(lapply(marketing[,1:4], normalize)) 

head(marketing.n)


# Creating Training and Test data set. 
# Training data will be used to build model whereas test data 
# will be used for validation and optimisation of model by 
# tuning k value.

set.seed(123)  # To get the same random sample
dat.d <- sample(1:nrow(marketing),size=nrow(marketing)*0.7,
                replace = FALSE) #random selection of 70% data.

train.marketing <- marketing.n[dat.d,] # 70% training data
test.marketing <- marketing.n[-dat.d,] # remaining 30% test data


# Fit the model on the training set using caret
# tuneLengh indicates 15 trials of K value

#1.- CREO EL MODELO PARA 15 Ks DISTINTAS
set.seed(123)
model <- train(
  sales~., data = train.marketing, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 15
)

#2.- PLOTTEO EL RMSE EN FUNCIÓN DE CADA K
plot(model)

#EL MEJOR MODELO // EL QUE TIENE MENOS RMSE
model$bestTune

#PREDICCIONES DEL MODELO (COGE AUTOMATICAMENTE EL BEST TUNE NO??)
predictions <- model %>% predict(test.marketing) #????
head(predictions)
#RMSE DE LAS PREDICCIONES
RMSE(predictions, test.marketing$sales)

#ERROR DE CADA PREDICCIÓN
error<- test.marketing$sales-predictions

#HISTOGRAMA DE LOS ERRORES
ggplot(data=data.frame(error), mapping= aes(x=error))+
  geom_histogram(binwidth=0.015, col=c('blue'))

#Residuals plotting
plot(error, col=c('blue'), type = "p", xlab = "Sample", 
     ylab = "Error Value", 
     main = "Errors between Real & Predicted values")

# Plot Real vs Predicted
plot(test.marketing$sales,type = "p",col = "red", xlab = "Sample", 
     ylab = "Sale Value", 
     main = "Sales: Real (red) - Predicted (blue)")

lines(predictions, type = "p", col = "blue")


