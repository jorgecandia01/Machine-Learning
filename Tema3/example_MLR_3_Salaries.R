#https://raw.github.com/vincentarelbundock/Rdatasets/master/csv/carData/Salaries.csv

# "","rank","discipline","yrs.since.phd","yrs.service","sex","salary"
#install.packages("tidyverse")      #data manipulation and visualization
#install.packages("readxl")   # For reading Excel files
#install.packages("psych")  # For multi.hist
#install.packages("car")  # For VIF

library("tidyverse")      #data manipulation and visualization
library("readxl")   # For reading Excel files
library("psych")  # For multi.hist
library("car")  # For VIF


# Load data to explore
# Conversion to factors
# salaries<-read.csv("Salaries.csv")
salaries<-
  read.csv("https://raw.githubusercontent.com/xkong100/IS607/master/Project2/Salaries.csv",
           stringsAsFactors = TRUE, check.names = TRUE, na.strings = c("", "NA"))
salaries$sex<-factor(salaries$sex)
salaries$rank<-factor(salaries$rank)
salaries$discipline<-factor(salaries$discipline)

# OR 
load("salaries.RData")

#OR  USE THIS
# Remove FALSE in stringAsFactor if factors are required
salaries<- read.csv("https://raw.githubusercontent.com/xkong100/IS607/master/Project2/Salaries.csv",
                  stringsAsFactors = TRUE,
                    check.names = TRUE, na.strings = c("", "NA"))

#Testing sefinition of factors
str(salaries)
summary(salaries)

#Basic representation of variables
#dcol color(s)for the normal and the density fits
#dlty typi of line for the normal and the density fits

multi.hist(x=salaries[,c(4:5,7)],dcol=c("blue","red"),
           dlty=c("dotted", "solid"), main=colnames(salaries[,c(4:5,7)]))

# scatterplots
pairs(~salaries$rank+salaries$discipline+salaries$yrs.since.phd+
        +salaries$yrs.service+salaries$sex+salaries$salary, 
      main='salary scatterplots', col=c('blue'))


salaries <- salaries[c(2:7)]
####################
#salary <- salaries$salary
#sex <- salaries$sex


#LM Model
model <- lm(salary ~ sex, data=salaries) #Si pongo el data no hace falta declarar variables
summary(model)

#coding that R have used to create the dummy variables
contrasts(salaries$sex)

#set the baseline category to males 
salaries <- salaries %>%
  mutate(sex = relevel(sex, ref = "Male"))

#Verification
contrasts(salaries$sex)

#LM Model
model_m <- lm(salary ~ sex, data=salaries)
summary(model_m) 

#LM model using -1 and 1 as dummy values
model_m2 = update(model_m, contrasts = list(sex = c(1,-1)))
summary(model_m2)

contrasts(salaries$sex)


#Adding another categorical variable RANK
#LM Model
model_rs <- lm(salary ~ sex+rank, data=salaries)
summary(model_rs)

#code that R have used to create the dummy variables
contrasts(salaries$rank)

vif(model_rs)


#Whole model 
#LM Model
model_full <- lm(salary ~ sex+rank+discipline+
                 yrs.since.phd+yrs.service, data=salaries)
summary(model_full)

vif(model_full)

contrasts(salaries$discipline)
contrasts(salaries$sex)
contrasts(salaries$rank)

#Coefficients
summary(model_full) $coefficient

#Interval of confidence for coefficients
confint(model_full)

#Histogram of residuals OPtion 1
hist(resid(model), main='Histogram of residuals',
     xlab='Standarised Residuals', ylab='Frequency)')

#Histogram of residuals OPtion 2
#ggplot(data=model_full, mapping= aes(x=model$residuals))+
#  geom_histogram(binwidth=0.5, col=c('blue'))

#Residuals vs Fitted
plot(model_full, which=1,  id.n=NULL, col=c('blue'))

#Residuals vs Fitted Another option
#qqnorm(model$residuals);qqline(model$residuals)

# Q-Q plot - Three more significant points
plot(model_full, which=2,  id.n=3, col=c('blue'))

# Cook distance - Five more significant points
plot(model_full, which=4,  id.n=5, col=c('blue'))


# Scale-Location plot
plot(model_full, which=3,  id.n=5, col=c('blue'))

# Residuals vs Leverage
plot(model_full, which=5,  id.n=3, col=c('blue'))


