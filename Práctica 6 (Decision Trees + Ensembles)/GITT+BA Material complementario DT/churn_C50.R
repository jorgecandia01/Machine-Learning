setwd("/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/Práctica 6 (Decision Trees + Ensembles)/GITT+BA Material complementario DT")

library(C50)
library(tidyverse)

load("churn_data.RData")

summary(churnTrain)

glimpse(churnTrain)

table(churnTrain$churn)

table(churnTest$churn)

table(churnTrain$churn)/nrow(churnTrain)

table(churnTest$churn)/nrow(churnTest)

corrplot_selection <- colnames(churnTrain)
corrplot_selection <- corrplot_selection[-20]
corrplot_selection <- corrplot_selection[-c(1,3,4,5)]
corrplot.mixed(corr=cor(churnTrain[, corrplot_selection],method="pearson"), 
               tl.pos="lt", tl.srt=5, addCoef.col = "black")

# Creating the decision tree algorithm C4.5 
tree_result <- C5.0(churn  ~ ., data=churnTrain,
                    control = C5.0Control(
                      noGlobalPruning = FALSE, # Pruning is in effect
                      CF= 0.25))  #Higher CF less prunning


summary(tree_result)

#PLotting the tree
plot(tree_result, trial=0, subtree=NULL)

#Prediction of new cases from the test dataset
predictions <- predict(tree_result, newdata = churnTest, type ="class")

table(prediction=predictions, real= churnTest$churn)

error_classification <- mean(predictions != churnTest$churn)

paste("The classification error is:", 100*error_classification, "%",
      sum(predictions==churnTest$churn),
      "correct classified cases from", length(predictions))

#Obtaining Knowledge rules

# Use this expression below
ruleModel <- C5.0(churn ~ ., data = churnTrain, rules = TRUE)

# Or the previous one with rules
#ruleModel <- C5.0(churn  ~ ., data=churnTrain,rules = TRUE,
#                    control = C5.0Control(
#                      noGlobalPruning = FALSE, # Pruning is in effect
#                      CF= 0.25))  #Higher CF less prunning



ruleModel

summary(ruleModel)


