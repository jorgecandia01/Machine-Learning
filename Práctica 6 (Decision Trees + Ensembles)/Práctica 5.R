setwd("/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/Práctica 5 (KNN- KMeans)")

library("corrplot")     #For corrplot
library(tidyverse)      #data manipulation and visualization
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

str(data)
summary(data)

corrplot_selection <- colnames(data)
corrplot.mixed(corr=cor(data[, corrplot_selection],method="pearson"), 
               tl.pos="lt", tl.srt=5, addCoef.col = "black")

#NORMALIZO LOS DATOS (aplico la función al subset de variables explicativas más tarde)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) } 

#SEPARO TRAIN Y TEST
set.seed(123)
#Escojo sólo estas variables (es lo que se ha considerado oportuno) y las normalizo
subdata <- as.data.frame(lapply(data[,c(1,4,5,8)], normalize)) 
#subdata <- normalizeData(data[,c(1,4,5,8)])
index <- sample(c(TRUE, FALSE), nrow(subdata), prob=c(0.7, 0.3), replace=TRUE)
train <- subdata[index,]
test <- subdata[!index,]
#La variable a predecir 
train_labels <- data[index,11]
test_labels  <- data[!index,11]




##### MÉTODO 1 -> KNN #####
#To identify optimum value of k, generally square root of total => k = 62
k <- sqrt(nrow(train)) 
knn.62 <-  knn(train=train, test=test, cl=train_labels, k=62) #CREACIÓN DEL MODELO
confusionMatrix(knn.62 ,as.factor(test_labels)) #RESULTADOS => Precisión del 94%

#Precisión de múltiples modelos para múltiples Ks, para encontrar la K óptima
i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in seq(1,80,by=2)){ 
  knn.mod <-  knn(train=train, test=test, cl=train_labels, k=i)
  k.optm[i] <- 100 * sum(test_labels == knn.mod)/NROW(test_labels)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

#Plot de la precisión de todas las Ks calculadas
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")  # to plot % accuracy wrt to k-value
#K con máxima precisión
k.max = match(max(k.optm),k.optm) # K = 3
#Model for optimal k value
knn.best <-  knn(train=train, test=test, cl=train_labels, k=k.max)
#RESULTADOS DEL MODELO ÓPTIMO
confusionMatrix(knn.best ,as.factor(test_labels)) #Precisión  del 96% con K = 3





##### MÉTODO 2 -> KMEANS #####
#Nº ÓPTIMO DE CLUSTERS, 3 MÉTODOS (Aunque realmente sabemos que son 5 clusters)
#Elbow method
#fviz_nbclust(train, kmeans, method = "wss", k.max = 20)
#Silhouette method
#fviz_nbclust(train, kmeans, method = "silhouette")
#GAP method
#gap_stat <- clusGap(train, FUN = kmeans, nstart = 25, K.max = 15, B = 25)
#fviz_gap_stat(gap_stat)
#print(gap_stat, method = "firstmax")

#IMPLEMENTACIÓN DEL ALGORITMO => 5 CLUSTERS, UNO POR CADA TIPO DE FIGURA
cluster_model <- kmeans(train,centers=5,nstart=500) #nstart es el máximo nº de actualización de centroides

#PREDICCIONES
cluster_predictor <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
} #Predicciones a partir de datos nuevos y el modelo
prediction<- cluster_predictor(test, cluster_model[["centers"]])
#Resultados de las predicciones
table_pred<-table(test_labels, prediction)
confusionMatrix(as.factor(test_labels), as.factor(prediction))

# #denormalizing centers to obtain real values
# denorm_centers<- denormalizeData(cluster_model$centers, getNormParameters(train))
# denorm_centers<-as.data.frame(denorm_centers)
# print(denorm_centers, digits=4)








































