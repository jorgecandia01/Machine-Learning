
library(tidyverse)      #data manipulation and visualization
library(class)          # to call class package for kNN

# Reading data
gc <- read.csv("german_credit.csv")

summary(gc)

# data structure where all the varaibles are integers including 
#'Creditability' which is the response variable for this example.
str(gc)

#Feature/Attribute selection

#The variable 'Creditability' is the target variable i.e. this 
#variable will determine whether bank manager will approve a loan 
#based on the 7 Attributes.

gc.subset <- gc[c('Creditability','Age..years.',
                  'Sex...Marital.Status','Occupation',
                  'Account.Balance','Credit.Amount',
                  'Length.of.current.employment','Purpose')]

head(gc.subset)

#Data normalistion to avoid biasness as the value class 
# of 'Credit.Amount'is in thousand whereas other attribute's value 
#are in 2 digits or 1 digit.

# creating a normalize function for easy convertion.
normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x))) } 

# lapply creates list (reason why it is converted to dataframe) and 
#it applies the defined function (which is 'normalize') to all 
#the list values which is here column 2 to 8 as first column 
#is target/response.
gc.subset.n<- as.data.frame(lapply(gc.subset[,2:8], normalize)) 

head(gc.subset.n)

#Now all attributes having value in the range 0 to 1 which is normalised
# data and 'Creditability' column has been removed 
#as sample value starts form column 2.

# Creating Training and Test data set. 
# Training data will be used to build model whereas test data 
# will be used for validation and optimisation of model by 
# tuning k value.

set.seed(123)  # To get the same random sample
dat.d <- sample(1:nrow(gc.subset.n),size=nrow(gc.subset.n)*0.7,
                replace = FALSE) #random selection of 70% data.

train.gc <- gc.subset[dat.d,] # 70% training data
test.gc <- gc.subset[-dat.d,] # remaining 30% test data

#Now creating separate dataframe for 'Creditability' feature 
#which is our target.
train.gc_labels <- gc.subset[dat.d,1]
test.gc_labels  <- gc.subset[-dat.d,1]
                             

# TRAINING the kNN model

#To identify optimum value of k, generally square root of total 
#no of observations (700) which is 26.45 is taken, 
#so 26, 27 will be tried as k value


sqrt(NROW(train.gc_labels))  # to find the number of observation

knn.26 <-  knn(train=train.gc, test=test.gc, cl=train.gc_labels, k=26)
knn.27 <-  knn(train=train.gc, test=test.gc, cl=train.gc_labels, k=27)

## Estimation of proportion of correct classification for k = 26, 27

ACC.26 <- 100 * sum(test.gc_labels == knn.26)/NROW(test.gc_labels)  # For knn = 26
ACC.27 <- 100 * sum(test.gc_labels == knn.27)/NROW(test.gc_labels)  # For knn = 27

ACC.26    #Accuracy is 67.67%
ACC.27    #Accuracy is 67.33%, which has reduced compare to k=26

# Checking prediction against actual value in tabular form
table(knn.26 ,test.gc_labels)  


# 29 & 174 are the correct prediction against actual wheras 
# 72 & 25 are wrong prediction against actual.

table(knn.27 ,test.gc_labels)  



library(caret)
confusionMatrix(knn.26 ,as.factor(test.gc_labels))

confusionMatrix(knn.27 ,as.factor(test.gc_labels))

i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in 1:28){ 
  knn.mod <-  knn(train=train.gc, test=test.gc, cl=train.gc_labels, k=i)
  k.optm[i] <- 100 * sum(test.gc_labels == knn.mod)/NROW(test.gc_labels)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

# Maximum accuracy at k=25   

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")  # to plot % accuracy wrt to k-value

#Model for optimal k value=25
knn.25 <-  knn(train=train.gc, test=test.gc, cl=train.gc_labels, k=25)

# Checking prediction against actual value in tabular form
table(knn.25 ,test.gc_labels)

#Croos-validation with training data set for k=25
knn.cross<-knn.cv(train=train.gc, cl=train.gc_labels, k=25,prob=TRUE)
table(knn.cross,train.gc_labels)

#Alternative calculation of accuracy
sum(knn.25 == test.gc_labels)/length(test.gc_labels)*100



