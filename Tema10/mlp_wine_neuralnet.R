#Data Set Information:
  
#These data are the results of a chemical analysis of wines 
#grown in the same region in Italy but derived from three different 
#cultivars. The analysis determined the quantities of 13 constituents 
#found in each of the three types of wines. 


#The attributes are (dontated by Riccardo Leardi, riclea '@' anchem.unige.it ) 
#1) Alcohol 
#2) Malic acid 
#3) Ash 
#4) Alcalinity of ash 
#5) Magnesium 
#6) Total phenols 
#7) Flavanoids 
#8) Nonflavanoid phenols 
#9) Proanthocyanins 
#10)Color intensity 
#11)Hue 
#12)OD280/OD315 of diluted wines 
#13)Proline 


# Attribute Information: All attributes are continuous 
#NOTE: 1st attribute is class identifier (1-3


library(tidyverse)  # for data processing
library(neuralnet)  # for MLP training
library(ggplot2)    #for plotting
library(dummies)  #for creration of dummy variables
library(caret)     #for confusion matrix


# Data collection from source
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
download.file(url, destfile = 'wine.csv', method = 'auto')

data <- read.csv('wine.csv', header = FALSE, sep=',')

# Viewing data collected
head(data)
str(data)

#assigning names for attributes from wine.names
names(data) <- c('Wine','Alcohol','Malic_acid','Ash',
                 'Alcalinity_ash','Magnesium','Total_phenols',
                 'Flavanoids','Nonflavanoinds_phenols',
                 'Proanthocyanins','Color_intensity',
                 'Hue','OD280_OD315_of_diluted_wines','Proline')

summary(data)

#scatterplots
pairs(data[2:14], col=data$Wine)

#plotting some specific relationships for cluster discovering
qplot(Alcohol, Flavanoids, data=data, 
      color=factor(data$Wine), geom=c("point","smooth"), 
      main = "Flavanoids and Alcohol in Wine")


qplot(Hue, Proline, data=data, color=factor(data$Wine), 
      geom=c("point","smooth"), main = "Proline and Hue in Wine")



#Creation of wine categories 1, 2 and 3
data_dm <- dummy.data.frame(data=data, names="Wine", sep="_")
str(data_dm)


#Normalization of variables
normalization <- function(x) {return ((x-min(x))/(max(x)-min(x)))}

data_norm <- as.data.frame(lapply(data_dm, normalization))
str(data_norm)

#Required if the same scenario is needed for new reproduction 
#of the code and obtaining the same resuts
set.seed(3141592) 

# creation of training and test datasets
index <- sample(nrow(data_norm), round(0.75*nrow(data_norm)))
train <- data_norm[index,] 
test <- data_norm[-index,]

#training the neural network
set.seed(3141592)
ann_model <- neuralnet(Wine_1+Wine_2+Wine_3~Alcohol+Malic_acid+
                         Ash+Alcalinity_ash+Magnesium+Total_phenols+
                         Flavanoids+Nonflavanoinds_phenols+
                         Proanthocyanins+Color_intensity+
                         Hue+OD280_OD315_of_diluted_wines+Proline,
                       data=train, hidden=c(5,5), 
                       lifesign = "minimal", 
                       linear.output = FALSE, rep =100)


# Visual plot of the model
plot(ann_model, rep="best")

# Best repetition with minimum error
best_rep<-which.min(ann_model$result.matrix[1,])


# Weights for the repetition with the best error
ann_model$weights[best_rep]

# activation function
ann_model$act.fct

# result.matrix: threshold, step, error, AIC, BIC and weights
#plotting the error per repetition
plot(ann_model$result.matrix[1,])
#threshold per repetition
plot(ann_model$result.matrix[2,])

#prediction with training set
pr.nn_tr <- predict(ann_model, train[,4:16], rep=best_rep,all.units=FALSE)
pr.nn_tr_round<-as.data.frame(round(pr.nn_tr))

pred_train<-max.col(pr.nn_tr_round)
real_train<-max.col(train[,1:3])

print(real_train)
print(pred_train)


# Plot real and predicted values for training
plot(real_train,type = "p",col = "red", xlab = "Sample", 
     ylab = "wine type", 
     main = "Training: Real (red) - Predicted (blue)")
lines(pred_train, type = "p", col = "blue")


caret::confusionMatrix(table(real_train,pred_train))
                

#prediction with TEST set
pr.nn_ts <- predict(ann_model, test[,4:16], rep=best_rep,all.units=FALSE)
pr.nn_ts_round<-as.data.frame(round(pr.nn_ts))

pred_ts<-max.col(pr.nn_ts_round)
real_ts<-max.col(test[,1:3])

# Plot real and predicted values for training
plot(real_ts,type = "p",col = "red", xlab = "Sample", 
     ylab = "wine type", 
     main = "Training: Real (red) - Predicted (blue)")
lines(pred_ts, type = "p", col = "blue")


print(real_ts)
print(pred_ts)
caret::confusionMatrix(table(real_ts,pred_ts))





