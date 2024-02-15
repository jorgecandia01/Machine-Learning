# Credit scoring application

library(RCurl)   # for using getURL
library(prettyR) # for nice printing
library(caret)   # for model regression
library(glmnet)  # for lasso regression

#loading data
url <- "https://raw.githubusercontent.com/gastonstat/CreditScoring/master/CleanCreditScoring.csv"
cs_data <- getURL(url)
cs_data <- read.csv(textConnection(cs_data))

describe(cs_data)
summary(cs_data)

classes <- cs_data[, "Status"]
predictors <- cs_data[, -match(c("Status", "Seniority", "Time", 
                                 "Age", "Expenses", "Income", "Assets", 
                                 "Debt", "Amount", "Price", "Finrat", 
                                 "Savings"), colnames(cs_data))]
# Data partition
train_set <- createDataPartition(classes, p = 0.8, list = FALSE)
str(train_set)

# Separating training and test datasets
train_predictors <- predictors[train_set, ] #Las variables descriptivas (sin la explicada)
train_classes <- classes[train_set] #El valor de la variable explicada
test_predictors <- predictors[-train_set, ]
test_classes <- classes[-train_set]

# Creation of CV folds
set.seed(123)
cv_splits <- createFolds(classes, k = 10, returnTrain = TRUE)
str(cv_splits)

# Preparation for regression
set.seed(1234)
cs_data_train <- cs_data[train_set, ]
cs_data_test <- cs_data[-train_set, ]

# Parameters for regularization
# Lambda  is the regularization parameter. SEveral values are tested
# alpha= 0 is RIDGE, alpha=1 is LASSO. Other intermedoate values are tested
#This variation of alpha is called mixing percentage
glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))

# CV method using 10 folds
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_fit <- train(Status ~ ., data = cs_data_train,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl)
# Results obtained
glmnet_fit

# best result
glmnet_fit$bestTune


# plotting results
trellis.par.set(caretTheme())
plot(glmnet_fit, scales = list(x = list(log = 2)))


# Prediction
#LO PREDICE CON EL BEST FIT O COMO?? Y YA CON LA P ÓPTIMA?? 0.5?? --> todo con lo óptimo
pred_classes <- predict(glmnet_fit, newdata = cs_data_test) 
test_classes <- factor(test_classes)
table(pred_classes)
confusionMatrix(test_classes, pred_classes)

# Probability to belong each test sample to the class bad or good
pred_probs <- predict(glmnet_fit, newdata = cs_data_test, type = "prob")
head(pred_probs)


# Rebuilding the model with best lamda value identified
glmnet_grid_best <- expand.grid(alpha = 0.6,lambda = 0.01)

# CV method using 10 folds
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_best <- train(Status ~ ., data = cs_data_train,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid_best,
                    trControl = glmnet_ctrl)

# Ideas for Obtaining the coeeficients of the model 
coef(glmnet_best)
glmnet_best$results





