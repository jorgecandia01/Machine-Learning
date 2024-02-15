setwd("/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/Práctica 6 (Decision Trees + Ensembles)/GITT+BA Material complementario DT")

library(C50)        # for obtaining data
library(tidyverse)  # for data processing
library(rpart)      # for CART decision tree
library(rpart.plot) # for plotting CART
library(caret)      # for confusion matrix and more


load("churn_data.RData")

summary(churnTrain)

glimpse(churnTrain)

table(churnTrain$churn)

table(churnTest$churn)

table(churnTrain$churn)/nrow(churnTrain)

table(churnTest$churn)/nrow(churnTest)

# Creating the decision tree algorithm in CART
tree_result<- rpart(formula=churn  ~ ., data=churnTrain, method='class')

#Resulting tree
print(tree_result)

# Alternative views
summary(tree_result)




#Fitting the plotting allowing labels in several lines
  split.fun <- function(x, labs, digits, varlen, faclen)
  {
    # replace commas with spaces (needed for strwrap)
    labs <- gsub(",", " ", labs)
    for(i in 1:length(labs)) {
      # split labs[i] into multiple lines
      labs[i] <- paste(strwrap(labs[i], width = 10), collapse = "\n")
    }
    labs
  }
  
#PLotting the tree
rpart.plot(tree_result, type=1, branch=0,tweak=2.3, 
             fallen.leaves = TRUE,
             varlen = 0, faclen = 0, split.fun=split.fun)
#Alternative for plotting the tree
prp(tree_result, faclen=3, clip.facs=TRUE, 
             split.fun=split.fun, tweak=1.2, extra=101)

################# #Prediction ############
#Poor option to plot the tree
plot(tree_result, uniform=TRUE, compress=TRUE)
text(tree_result, use.n=TRUE)

plot(tree_result, subtree=3)

#Obtainimng the decision rules from the tree
rpart.rules(tree_result, style = "tall", cover=TRUE,
            nn=TRUE, clip.facs = TRUE)
            

#Prediction of the training cases from the train dataset
pred_train <- predict(tree_result, newdata = churnTrain, type ="class")
confusionMatrix(pred_train, churnTrain$churn)

table(prediction=pred_train, real= churnTrain$churn)

error_classification <- mean(pred_train != churnTrain$churn)

paste("The classification error is:", 100*error_classification, "%",
      sum(pred_train==churnTrain$churn),
      "correct classified cases from", length(pred_train))

#Prediction of new cases from the test dataset
predictions <- predict(tree_result, newdata = churnTest, type ="class")
confusionMatrix(predictions, churnTest$churn)

table(prediction=predictions, real= churnTest$churn)

error_classification <- mean(predictions != churnTest$churn)

paste("The classification error is:", 100*error_classification, "%",
      sum(predictions==churnTest$churn),
      "correct classified cases from", length(predictions))



#Prunning analysis
tree_pruned<- prune(tree_result, cp=0.02)
rpart.plot(tree_pruned, type=1, branch=0,tweak=2.3, 
           fallen.leaves = TRUE,
           varlen = 0, faclen = 0, split.fun=split.fun)


#Prediction of the training cases from the train dataset PRUNING
pred_train <- predict(tree_pruned, newdata = churnTrain, type ="class")
confusionMatrix(pred_train, churnTrain$churn)

table(prediction=pred_train, real= churnTrain$churn)

error_classification <- mean(pred_train != churnTrain$churn)

paste("The classification error is:", 100*error_classification, "%",
      sum(pred_train==churnTrain$churn),
      "correct classified cases from", length(pred_train))

#Prediction of new cases from the test dataset
predictions <- predict(tree_pruned, newdata = churnTest, type ="class")
confusionMatrix(predictions, churnTest$churn)

table(prediction=predictions, real= churnTest$churn)

error_classification <- mean(predictions != churnTest$churn)

paste("The classification error is:", 100*error_classification, "%",
      sum(predictions==churnTest$churn),
      "correct classified cases from", length(predictions))



# Analysis of cp values in a table
printcp(tree_result, digits=6)

#Obtaining the best cp 
best_cp<- tree_result$cptable[which.min(tree_result$cptable[,"xerror"]),"CP"]

# Error evolution with increasing number of nodes
plotcp(tree_result, lty=2 , col="red", upper="size" )
plotcp(tree_result, lty=2 , col="red", upper="splits" )


#Prunning analysis with best_cp
tree_pruned<- prune(tree_result, cp=best_cp)
rpart.plot(tree_pruned, type=1, branch=0,tweak=1.8, 
           fallen.leaves = TRUE,
           varlen = 0, faclen = 0, split.fun=split.fun)


#Prediction of the training cases from the train dataset PRUNING
pred_train <- predict(tree_pruned, newdata = churnTrain, type ="class")
confusionMatrix(pred_train, churnTrain$churn)

table(prediction=pred_train, real= churnTrain$churn)

error_classification <- mean(pred_train != churnTrain$churn)

paste("The classification error is:", 100*error_classification, "%",
      sum(pred_train==churnTrain$churn),
      "correct classified cases from", length(pred_train))

#Prediction of new cases from the test dataset
predictions <- predict(tree_pruned, newdata = churnTest, type ="class")
confusionMatrix(predictions, churnTest$churn)

table(prediction=predictions, real= churnTest$churn)

error_classification <- mean(predictions != churnTest$churn)

paste("The classification error is:", 100*error_classification, "%",
      sum(predictions==churnTest$churn),
      "correct classified cases from", length(predictions))





