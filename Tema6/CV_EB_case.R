library(tidyverse)
library(caret)

EB <- read_csv("https://raw.githubusercontent.com/zilinskyjan/datasets/master/economic_sentiment/eurobarometer.csv")

# Estimate a basic regression model
ols_basic <- lm(eb_econgood ~ gdpgrowth + unemployment, data=EB)

summary(ols_basic)

set.seed(1234)
#Estimation using CV with 5 folds
ols_cv <- train(eb_econgood~ gdpgrowth + unemployment,
                data = EB, 
                method = "lm",
                trControl=trainControl(
                  method = "cv",
                  number=5,
                  savePredictions = TRUE,
                  verboseIter = TRUE))
                
summary(ols_cv)

# How good is the model?
ols_cv$results

# Showing predictions 
ols_cv$pred

# Data corresponding to row 2 in EB
EB[2,]

# Showing the final prediction
predict(ols_cv)[2]







ols_cv$resample



