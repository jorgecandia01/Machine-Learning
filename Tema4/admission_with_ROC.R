#  Packages
#library(tidyverse)      #data manipulation and visualization
#library("plot3D")
library("psych")  # for multi-hist
library(aod)
library(ggplot2)
library(vcd) # For mosaic
library(ROCR) #For ROC curve and lift chart

(tilde <- rawToChar(as.raw(126)))

#Loading data
data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

#Inspection of basic data characteristics
summary(data)


#Basic representation of variables
#dcol color(s)for the normal and the density fits
#dlty type of line for the normal and the density fits

multi.hist(x=data[,1:4],dcol=c("blue","red"),
           dlty=c("dotted", "solid"), main=colnames(data))

#Conversion of rank to a factor to indicate that
#rank should be treated as a categorical variable

data$rank<- factor(data$rank)

# Logistic Regression Model with 3 independent inputs
log_model<-glm(admit~gre+gpa+rank, family="binomial", data=data)

summary(log_model)

#Confidence Intervals for coefficients using profiled log-likelihood
confint(log_model)

#Confidence Intervals for coefficients using standarerrors
confint.default(log_model)

#Test of fit
#Difference in deviance of the models without and with predictors
with(log_model, null.deviance -deviance)

#degrees of freedom
with(log_model, df.null-df.residual)

#p value of degrees of freedom
with(log_model, pchisq(null.deviance -deviance, df.null-df.residual,
                       lower.tail=FALSE))

# Wald test for the variable rank
wald.test(b=coef(log_model), Sigma=vcov(log_model), Terms=4:6)

# Testing the different between rank 2 and rank 3
# Creation of the vector needed 
vector<-cbind(0,0,0,1, -1, 0)
wald.test(b=coef(log_model), Sigma=vcov(log_model), L=vector)

#Obtaining the odd ratios 
exp(coef(log_model))

#Obtaining the odd ratios and their confident intervals
exp(cbind(OR= coef(log_model), confint(log_model)))

#Creation of a new data frame to be used in prediction
new_df<-with(data, data.frame(gre=mean(gre), gpa=mean(gpa), 
                              rank = factor(1:4)))
#view of the new data frame
new_df

#Predicting ranks using mean values for gre and gpa
new_df$rankP<-predict(log_model, newdata=new_df, type="response")

#Showing the predicted values
new_df


#Creation of a new second data frame to be used in prediction
new_df2<-with(data, data.frame(gre=rep(seq(from=200, to=800, 
                                           length.out=100), 4), gpa=mean(gpa),
                               rank=factor(rep(1:4, each=100))))

#Creation of a third data frame to be used in prediction
# se (standar error) are requested
new_df3<-cbind(new_df2, predict(log_model, 
                                newdata=new_df2, type="link", se=TRUE))

#Obtaining prediction and confidence intervals
new_df3<-within(new_df3, {
  PredictedProb<- plogis(fit)
  LL<- plogis(fit-(1.96*se.fit))
  UL<- plogis(fit+(1.96*se.fit))
})

#presenting results
head(new_df3)

#Plotting the results
ggplot(new_df3, aes(x = gre, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL,
                  ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
                                                                    size = 1)



#Confussion in the training set
predictions<-ifelse(test=log_model$fitted.values<0.4,yes=0, no=1)

confusion_matrix<-table(log_model$model$admit,predictions,
                        dnn=c("observations", "predictions"))

confusion_matrix

#plotting the confussion matrix
mosaic(confusion_matrix,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))



# Plot the performance of the model applied to the evaluation set as
# an ROC curve.
library(ROCR)
#predict<-ifelse(test=log_model$fitted.values<0.4,yes=0, no=1)

pred <- prediction(predictions= log_model$fitted.values,
                   labels = log_model$model$admit)
perf <- performance(pred,"tpr","fpr")
#plot(perf,lwd=2,col='blue',main="ROC Curve")
plot(perf,main='ROC curve', colorize=TRUE)
abline(a=0, b= 1)




# Punto de corte óptimo del clasificador
cost.perf <- performance(pred, measure ="cost")
opt.cut   <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
#coordenadas del punto de corte óptimo
x<-perf@x.values[[1]][which.min(cost.perf@y.values[[1]])]
y<-perf@y.values[[1]][which.min(cost.perf@y.values[[1]])]
points(x,y, pch=20, col="red")

plot(perf,colorize=TRUE,type="l") 
abline(a=0,b=1)
points(x,y, pch=20, col="red")

# Área bajo la curva
AUC       <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
AUCaltura

cat("AUC:", AUCaltura[[1]]) 

cat("Punto de corte óptimo:",opt.cut)



predictions<-ifelse(test=log_model$fitted.values<opt.cut,yes=0, no=1)

confusion_matrix<-table(log_model$model$admit,predictions,
                        dnn=c("observations", "predictions"))

confusion_matrix



#liftcurve 
library(lift) #for lift curve
plotLift(log_model$fitted.values,log_model$model$admit, cumulative = TRUE, 
         n.buckets = 10)
TopDecileLift(log_model$fitted.values,log_model$model$admit)

# Alternative ROC curve plottings

# Plot the performance of the model applied to the evaluation set as
# an ROC curve.

pred1 <- prediction(predictions= log_model$fitted.values,
                    labels = log_model$model$admit)
perf1 <- performance(pred1,"tpr","fpr")
plot(perf1, main="ROC curve", colorize=T)
abline(a=0, b= 1)

# And then a lift chart
perf2 <- performance(pred1,"lift","rpp")
plot(perf2, main="lift curve", colorize=T)

