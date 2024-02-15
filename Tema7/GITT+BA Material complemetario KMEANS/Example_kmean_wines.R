#These data are the results of a chemical analysis of wines grown 
#in the same region in Italy but derived from three different 
#cultivars. The analysis determined the quantities of 13 constituents 
#found in each of the three types of wines.
# link at https://archive.ics.uci.edu/ml/datasets/wine


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dummies)  #for creration of dummy variables
library(caret)     #for confusion matrix
library(RSNNS)     #for normalization

# Data collection from source
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
download.file(url, destfile = 'wine.csv', method = 'auto')

data <- read.csv('wine.csv', header = FALSE, sep=',')

# Viewing data collected
head(data)
str(data)

#assigning names for attributes from wine.names
names(data) <- c('Wine','Alcohol','Malic_acid','Ash',
                 'Alcalinity_ash','Magnesium','Total_phenols',
                 'Flavanoids','Nonflavanoinds_phenols',
                 'Proanthocyanins','Color_intensity',
                 'Hue','OD280_OD315_of_diluted_wines','Proline')

summary(data)

#Creation of data using atributtes 2 to 14. Column 1 is for wine type
data_dm <- dummy.data.frame(data=data[,2:14], names="Wine", sep="_")
str(data_dm)


#Scaling variables
#data.sc <-scale(data_dm)
#attr(data.sc,"scaled:center")
#attr(data.sc,"scaled:scale")



#Normalization of variables
#values <- normalizeData(data)
#denormalizeData(values, getNormParameters(values))

#Using a customer function
#normalization <- function(x) {return ((x-min(x))/(max(x)-min(x)))}
#data_norm <- as.data.frame(lapply(data_dm, normalization))

#Normalization of variables
data_norm_or<-normalizeData(data_dm, type="0_1" )
getNormParameters(data_norm_or)

#conversion to data frame
data_norm<- as.data.frame(data_norm_or)

#Adding names to the columns
names(data_norm) <- c('Alcohol','Malic_acid','Ash',
                 'Alcalinity_ash','Magnesium','Total_phenols',
                 'Flavanoids','Nonflavanoinds_phenols',
                 'Proanthocyanins','Color_intensity',
                 'Hue','OD280_OD315_of_diluted_wines','Proline')
str(data_norm)
head(data_norm)


#Required if the same scenario is needed for new reproduction 
#of the code and obtaining the same resuts
set.seed(3141592) 

# creation of training and test datasets
index <- sample(nrow(data_norm), round(0.75*nrow(data_norm)))
train <- data_norm[index,] 
test <- data_norm[-index,]
train_label<- data[index,1]
test_label<- data[-index,1]

#Clustering using kmeans

#Optimum number of clusters. Elbow method
# Alternative using fviz function for Elbow method
#set.seed(123)
fviz_nbclust(train, kmeans, method = "wss", k.max = 20)

#Optimum number of clusters. Silhouette method
# function to compute average silhouette for k clusters
#set.seed(123)
fviz_nbclust(train, kmeans, method = "silhouette")

#GAP method
# compute gap statistic
#set.seed(123)
gap_stat <- clusGap(train, FUN = kmeans, nstart = 25,
                    K.max = 15, B = 25)
# Print the result
print(gap_stat, method = "firstmax")

# Alternative using the fviz_gap_stat function
fviz_gap_stat(gap_stat)


########## 3 clusters one per type of wine
cluster3<-kmeans(train,centers=3,nstart=20)

# clustering results
str(cluster3)
cluster3

#visualization  of clusters 
fviz_cluster(cluster3, data=train,
             choose.vars = colnames(train[, c(1,7)]))


fviz_cluster(cluster3, data=train,
             choose.vars = colnames(train[, c(1,7)]),stand = FALSE, 
             ellipse.type = "norm") + theme_bw()

######## Analysis of clusters
datac1 <- cluster3$cluster 

data_clus_1 <- train[datac1 == 1,] #distance to the assigned cluster
data_clus_1L <- train_label[datac1 == 1] #type of wine in cluster 1
data_clus_1L
data_clus_2 <- train[datac1 == 2,]
data_clus_2L <- train_label[datac1 == 2] #type of wine in cluster 2
data_clus_2L
data_clus_3 <- train[datac1 == 3,]
data_clus_3L <- train_label[datac1 == 3] #type of wine in cluster 3
data_clus_3L

############ Prediction of clusters and wine type. Test dataset
clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}


#all.equal(cluster3[["cluster"]], clusters(test, cluster3[["centers"]]))

# new data to predict
prediction<- clusters(test, cluster3[["centers"]])

#colnames(prediction) <- colnames(test)

table_pred<-table(test_label, prediction)
confusionMatrix(as.factor(test_label), as.factor(prediction))

#denormalizing centers to obtain real values
denorm_centers<- denormalizeData(cluster3$centers, getNormParameters(data_norm_or))

denorm_centers<-as.data.frame(denorm_centers)
names(denorm_centers) <- c('Alcohol','Malic_acid','Ash',
                      'Alcalinity_ash','Magnesium','Total_phenols',
                      'Flavanoids','Nonflavanoinds_phenols',
                      'Proanthocyanins','Color_intensity',
                      'Hue','OD280_OD315_of_diluted_wines','Proline')
denorm_centers

# Controlling the digits to show
print(denorm_centers, digits=4)







