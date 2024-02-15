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
#library(modelr)         #provides easy pipeline modeling
#library(broom)          #helps to tidy up model outputs
library("plot3D")
library("psych")
library("corrplot")

#Loading marketing data
load("marketing.rda")

#Inspection of basic characteristics of data
summary(marketing)

#Basic representation of variables
#dcol color(s)for the normal and the density fits
#dlty type of line for the normal and the density fits

multi.hist(x=marketing[,1:4],dcol=c("blue","red"),
           dlty=c("dotted", "solid"), main=colnames(marketing))


#Creating isolated variables
youtube<-marketing$youtube
facebook<-marketing$facebook
newspaper<-marketing$newspaper
sales<-marketing$sales

# scatterplots
pairs(~sales+youtube+facebook+newspaper, 
      main='marketing scatterplots', col=c('blue'))

# Table with Pearson's correlation coefficients

#Round to 2 digit
#round(cor(cbind(marketing$sales,marketing$youtube, 
#                marketing$facebook,marketing$newspaper)),2)
#Round to 4 digit
#round(cor(cbind(sales,youtube, facebook,newspaper)),4)

#Correlation with colors
corrplot.mixed(corr=cor(marketing[, colnames(marketing)], 
              method="pearson"), tl.pos="lt", tl.srt=45, 
              addCoef.col = "black")         


#Printing linear relationship sales ~ youtube and facebook
#points3D(youtube,facebook,sales, colvar =sales, col='blue')


#Printing linear relationship sales ~ youtube and facebook
scatter3D(youtube, facebook, sales, pch=20, cex=0.8, theta=30, 
          phi=18, bty="b2", col=c('red'), ticktype='simple', 
          xlab="Youtube", ylab="Facebook", 
          zlab="Sales", main="Marketing")




#MOdelling
model<-lm(sales~youtube+facebook+newspaper)

#Results
summary(model)

#Coefficients
summary(model) $coefficient

#Interval of confidence for coefficients
confint(model)

#Histogram of residuals OPtion 1
#hist(resid(model), main='Histogram of residuals',
#     xlab='Standarised Residuals', ylab='Frequency)')

#Histogram of residuals OPtion 2
ggplot(data=model, mapping= aes(x=model$residuals))+
  geom_histogram(binwidth=0.5, col=c('blue'))

#Residuals vs Fitted
plot(model, which=1,  id.n=NULL, col=c('blue'))

#Residuals vs Fitted Another option
#qqnorm(model$residuals);qqline(model$residuals)

# Q-Q plot - Three more significant points
plot(model, which=2,  id.n=3, col=c('blue'))

# Cook distance - Five more significant points
plot(model, which=4,  id.n=5, col=c('blue'))


# Scale-Location plot
plot(model, which=3,  id.n=5, col=c('blue'))

# Residuals vs Leverage
plot(model, which=5,  id.n=3, col=c('blue'))




################# MODEL SIMPLIFIED 

#Modelling- Model simplified
model_s<-lm(sales~youtube+facebook)

#Results
summary(model_s)

#Histogram of residuals OPtion 2
ggplot(data=model_s, mapping= aes(x=model_s$residuals))+
  geom_histogram(binwidth=0.5, col=c('blue'))

#Residuals vs Fitted
plot(model_s, which=1,  id.n=NULL, col=c('blue'))


# Q-Q plot - Three more significant points
plot(model_s, which=2,  id.n=3, col=c('blue'))

# Cook distance - Five more significant points
plot(model_s, which=4,  id.n=5, col=c('blue'))


#model prediction
fitpoints<-predict(model_s)

# Plot the bar chart. (p point, l lines, o both)
plot(sales,type = "p",col = "red", xlab = "Sample", ylab = "Sale Value", 
     main = "Sales: Real (red) - Predicted (blue)")

lines(fitpoints, type = "p", col = "blue")

#Residuals
res<-sales-fitpoints

# Plot the bar chart of residuals. (p point, l lines, o both)
plot(res,type = "p",col = "red", xlab = "Sample", ylab = "Residual Value", 
     main = "Sale Residuals: Real - Predicted Values")


# Q-Q plot - Three more significant points
plot(model_s, which=2,  id.n=3, col=c('blue'))

# Cook distance - Five more significant points
plot(model_s, which=4,  id.n=5, col=c('blue'))

# Scale-Location plot
plot(model_s, which=3,  id.n=5, col=c('blue'))

# Residuals vs Leverage
plot(model_s, which=5,  id.n=3, col=c('blue'))

