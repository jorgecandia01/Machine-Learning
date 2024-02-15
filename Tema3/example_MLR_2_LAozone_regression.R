#These data record the level of atmospheric ozone concentration from
#eight daily meteorological measurements made in the Los Angeles basin
#in 1976.  Although measurements were made every day that year, some
#observations were missing; here we have the 330 complete cases.  The
#data were given to us by Leo Breiman; he was a consultant on a project
#from which these data are taken.  The response, referred to as ozone,
#is actually the log of the daily maximum of the hourly-average ozone
#concentrations in Upland, California.

#Detailed variable names:

#ozone : Upland Maximum Ozone
#vh : Vandenberg 500 mb Height
#wind : Wind Speed (mph)
#humidity : Humidity (%)
#temp : Sandburg AFB Temperature
#ibh : Inversion Base Height
#dpg : Daggot Pressure Gradient
#ibt : Inversion Base Temperature
#vis : Visibility (miles)
#doy : Day of the Year

#  Packages
library("tidyverse")      #data manipulation and visualization
library("car")  # For VIF
library("psych")  # For multi.hist
library("corrplot") #For corrplot
library("plot3D")
library("modelr")  # For add_predictions

LAozone = read.table("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/LAozone.data",sep=",",head=T)

#Inspection of basic characteristics of data
summary(LAozone)

#Basic representation of variables
#dcol color(s)for the normal and the density fits
#dlty typi of line for the normal and the density fits

multi.hist(x=LAozone[,1:6],dcol=c("blue","red"),
           dlty=c("dotted", "solid"), main=colnames(LAozone))

multi.hist(x=LAozone[,7:10],dcol=c("blue","red"),
           dlty=c("dotted", "solid"), main=colnames(LAozone[,7:10]))

# scatterplots
pairs(~LAozone$ozone+LAozone$vh+LAozone$wind+LAozone$humidity
      +LAozone$temp+LAozone$ibh+LAozone$dpg+LAozone$ibt+
        LAozone$vis+LAozone$doy, 
      main='LAozone scatterplots', col=c('blue'))

# Table with Pearson's correlation coefficients

#Round to 2 digit
round(cor(cbind(LAozone$ozone,LAozone$vh,LAozone$wind,
                LAozone$humidity,LAozone$temp,LAozone$ibh,
                LAozone$dpg, LAozone$ibt,LAozone$vis,LAozone$doy)),2)
round(cor(LAozone),2)

#Correlation with colors
corrplot.mixed(corr=cor(LAozone[, colnames(LAozone)], 
                        method="pearson"), tl.pos="lt", tl.srt=45)

#LM Model
model<-lm(formula=ozone ~., data=LAozone)
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


#Measuring collinearity
vif(model)





################# MODEL SIMPLIFIED 1 STEP

humidity=LAozone$humidity
temp=LAozone$temp
ibt=LAozone$ibt
vis=LAozone$vis
doy=LAozone$doy
ozone=LAozone$ozone


#Modelling- Model simplified
model_s<-lm(ozone~humidity+temp+ibt+vis+doy)

#Results
summary(model_s)

#Interval of confidence for coefficients
confint(model_s)

#Measuring collinearity
vif(model_s)

# scatterplots
pairs(~ozone+humidity+temp+ibt+vis+doy, 
      main='LAozone scatterplots simplified', col=c('blue'))

#Correlation rounded to 2 digit
round(cor(cbind(ozone,humidity,temp,ibt,vis,doy)),2)



################# MODEL SIMPLIFIED 2 STEP

#Modelling- Model simplified
model_s2<-lm(ozone~humidity+temp+vis+doy)

#Results
summary(model_s2)

#Interval of confidence for coefficients
confint(model_s2)

#Measuring collinearity
vif(model_s2)

# scatterplots
pairs(~ozone+humidity+temp+vis+doy, 
      main='LAozone scatterplots simplified', col=c('blue'))

#Correlation rounded to 2 digit
round(cor(cbind(ozone,humidity,temp, vis,doy)),2)


################# MODEL SIMPLIFIED 2 STEP with interaction

#Modelling- Model simplified
model_s3<-lm(ozone~humidity+vis+doy+humidity*temp)

#Results
summary(model_s3)

#Interval of confidence for coefficients
confint(model_s3)

#Measuring collinearity
vif(model_s3)

# scatterplots
pairs(~ozone+humidity+temp+vis+doy+temp_hum, 
      main='LAozone scatterplots simplified', col=c('blue'))

#Correlation rounded to 2 digit
round(cor(cbind(ozone,humidity,temp, vis,doy)),2)



#########################PREDICTION FROM SCRATCH

#Creation of a training and test sets with the original data
set.seed(123)
sample <- sample(c(TRUE, FALSE), 
                 nrow(LAozone), replace = T, prob = c(0.7,0.3))
train <- LAozone[sample, ]
test <- LAozone[!sample, ]


################# Training and TESTING MOdel_s3


#Modelling- Model simplified
model_s3tr<-lm(ozone~humidity+vis+doy+humidity*temp, data=train)

#Results
summary(model_s3tr)

#Interval of confidence for coefficients
confint(model_s3tr)


## Two alternatives for prediction

predictions = predict(model_s3tr, new=test)

## This option adds the prediction in the last column of test
#(test<-test %>% add_predictions(model_s3tr)) #funciona y a?ade en vector test


# Plot the bar chart. (p point, l lines, o both)
plot(test$ozone,type = "p",col = "red", xlab = "Sample", 
     ylab = "Ozone Value", 
     main = "Ozone: Real (red) - Predicted (blue)")

## lines(test$pred, type = "p", col = "blue") #  option for add_prediction
lines(predictions, type = "p", col = "blue")

#Residuals
#res<-test$ozone-test$pred #  option for add_prediction
res<-test$ozone-predictions

# Plot the bar chart of residuals. (p point, l lines, o both)
plot(res,type = "p",col = "red", xlab = "Sample", ylab = "Residual Value", 
     main = "Ozone Residuals: Real - Predicted Values")


#Histogram of residuals OPtion 2
ggplot(data=model_s3tr, mapping= aes(x=model_s3tr$residuals))+
  geom_histogram(binwidth=0.5, col=c('blue'))

#Residuals vs Fitted
plot(model_s3tr, which=1,  id.n=NULL, col=c('blue'))


# Q-Q plot - Three more significant points
plot(model_s3tr, which=2,  id.n=3, col=c('blue'))


# Cook distance - Five more significant points
plot(model_s3tr, which=4,  id.n=5, col=c('blue'))

# Scale-Location plot
plot(model_s3tr, which=3,  id.n=5, col=c('blue'))

# Residuals vs Leverage
plot(model_s3tr, which=5,  id.n=3, col=c('blue'))

