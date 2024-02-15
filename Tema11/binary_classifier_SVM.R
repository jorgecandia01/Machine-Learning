
library(ggplot2) # Required for plotting with ggplot
library(e1071) #Required for SVM
library(vcd) # For mosaic

# Creation of an artificial data set not linearly separable
set.seed(10111)
coordinates<-matrix(rnorm(40), 20, 2)
colnames(coordinates)<-c("X1", "X2")

y<-c(rep(-1,10),rep(1,10))

coordinates[y ==1, ] <- coordinates[y==1, ] +1
data<-data.frame(coordinates, y)

ggplot(data=data,aes(x=X1, y=X2, color=as.factor(y)))+
  geom_point(size=6)+
  theme_bw()+theme(legend.position ="none")

#Creation of a simple Support Vector Classifier 
# Conversion of y variable to factor
data$y<-as.factor(data$y)

# Creating the SVC model. Use kernel as "linear"
model_svc<-svm(formula=y ~ X1+X2, data=data, kernel="linear", 
               cost=10, scale=FALSE)

summary(model_svc)

#Support vectors
model_svc$index 

#Plotting the two regions in low resolution
plot(model_svc, data)


#Evaluation of the optimum  cost value 
set.seed(1)
svc_cv<-tune("svm", y ~ X1+X2, data=data, kernel="linear",
             ranges=list(cost=c(0.001, 0.01, 0.1, 
                                1, 5, 10, 20, 50, 100, 150, 200)))


summary(svc_cv)

# Plotting error versus cost
ggplot(data=svc_cv$performances, aes(x=cost, y=error))+geom_line()+
  geom_point()+
  labs(title="Classification error vs hyperparameter C (cost)")+
  theme_bw()

#Saving the best model found for C
best_model <- svc_cv$best.model

#Prediction of new observations
set.seed(19)
coordinates <-matrix(rnorm(40), 20, 2)
colnames(coordinates) <-c("X1", "X2")

y<-sample(c(-1,1), 20, rep=TRUE)

coordinates[y ==1, ] <- coordinates[y==1, ] +1
test<-data.frame(coordinates, y)

#Predictions
predictions<- predict(object=best_model, test)
paste("Test error:", 100*mean(test$y !=predictions), "%")

table(prediction=predictions, real_value=test$y)
      
#Alternative way
confusion_matrix<-table(test$y,predictions,
                        dnn=c("real values", "predictions"))

confusion_matrix

#plotting the confusion matrix
mosaic(confusion_matrix,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))










