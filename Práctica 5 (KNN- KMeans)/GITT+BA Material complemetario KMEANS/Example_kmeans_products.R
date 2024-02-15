#Data Set Information:
  
#  Provide all relevant information about your data set.


#Attribute Information:
  
#1)	FRESH: annual spending (m.u.) on fresh products (Continuous); 
#2)	MILK: annual spending (m.u.) on milk products (Continuous); 
#3)	GROCERY: annual spending (m.u.)on grocery products (Continuous); 
#4)	FROZEN: annual spending (m.u.)on frozen products (Continuous) 
#5)	DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous) 
#6)	DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous); 
#7)	CHANNEL: customers????T Channel - Horeca (Hotel/Restaurant/Café) or Retail channel (Nominal) 
#8)	REGION: customers????T Region ????" Lisnon, Oporto or Other (Nominal) 
#Descriptive Statistics: 
  
#REGION	Frequency 
#Lisbon	77 
#Oporto	47 
#Other Region	316 
#Total	440 

#CHANNEL	Frequency 
#Horeca	298 
#Retail	142 
#Total	440 


setwd('/Users/jorgecandia/UNIVERSIDAD/TERCERO/Machine Learning/Prácticas R/Práctica 5 (KNN- KMeans)/GITT+BA Material complemetario KMEANS')

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(DMwR2)       # for unscale

data <- read.csv('Wholesale_customers_data.csv', header = TRUE, sep=',')

# Viewing data collected
head(data)
str(data)


summary(data)

#removing any missing value
data<-na.omit(data)

#scaling data
data<-scale(data)

#transposing the data matrix for getting distances
tdata<-t(data)

# computing distance matrix between the rows of the data matrix
distance <- get_dist(tdata)
# Visualization of a distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", 
                                    mid = "white", high = "#FC4E07"))


#Clustering using kmeans
########## 2 clusters
cluster2<-kmeans(data,centers=2,nstart=25) #nstart eran los saltos maximos o algo asi

# clusytering results
str(cluster2)
cluster2

###SE SOLAPAN PORQUE ES UNA REPRESENTACIÓN EN 2D DE MAS DIMENSIONES
#ESTAMOS PLOTTEANDO LAS PROYECCIONES DE LOS CLUSTERS EN DISTINTOS PLANOS (asi se obtiene info//reglas del conocimiento)
#visualization  of clusters 
fviz_cluster(cluster2, data=data,
             choose.vars = c("Grocery", "Frozen"))


fviz_cluster(cluster2, data=data,
             choose.vars = c("Channel", "Region"),stand = FALSE, 
             ellipse.type = "norm") + theme_bw()


fviz_cluster(cluster2, data=data,
             choose.vars = c("Fresh", "Milk"),stand = FALSE, 
             ellipse.type = "norm") + theme_bw()

fviz_cluster(cluster2, data=data,
             choose.vars = c("Grocery", "Frozen"),stand = FALSE, 
             ellipse.type = "norm") + theme_bw()

fviz_cluster(cluster2, data=data,
             choose.vars = c("Detergents_Paper", "Delicassen"),stand = FALSE, 
             ellipse.type = "norm") + theme_bw()


fviz_cluster(cluster2, data=data,
             choose.vars = c("Detergents_Paper", "Grocery"),stand = FALSE, 
             ellipse.type = "norm") + theme_bw()

fviz_cluster(cluster2, data=data,
             choose.vars = c("Fresh", "Frozen"))

# Another clustering method of visualization

data %>%
  as_tibble() %>%
  mutate(cluster = cluster2$cluster,
         state = row.names(data)) %>%
  ggplot(aes(Grocery, Frozen, color = factor(cluster), label = state)) +
  geom_text()


########## 3 clusters
cluster3<-kmeans(data,centers=3,nstart=25)

# clusytering results
str(cluster3)
cluster3

#visualization  of clusters 
fviz_cluster(cluster3, data=data, choose.vars = c("Grocery", "Frozen"))

fviz_cluster(cluster3, data=data, 
             choose.vars = c("Grocery", "Frozen"),
             stand = FALSE, ellipse.type = "norm") + theme_bw()


########## 4 clusters
cluster4<-kmeans(data,centers=4,nstart=25)

# clusytering results
str(cluster4)
cluster4

#visualization  of clusters 
fviz_cluster(cluster4, data=data)

#Optimum number of clusters. Elbow method
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 25
k.values <- 1:25

# extract wss for 1-25 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Alternative using fviz function for Elbow method
set.seed(123)
fviz_nbclust(data, kmeans, method = "wss", k.max = 20)



########## 8 clusters
cluster8<-kmeans(data,centers=9,nstart=25)

# clusytering results
str(cluster8)
cluster8

#visualization  of clusters 
fviz_cluster(cluster8, data=data)


#Optimum number of clusters. Silhouette method
#SE SUELE PREFERIR EL MENOR Nº DE CLUSTERS
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(data, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 20
k.values <- 2:20

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

# Alternative using fviz function for Silhouette method

fviz_nbclust(data, kmeans, method = "silhouette")

#GAP method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(data, FUN = kmeans, nstart = 25,
                    K.max = 15, B = 25)
# Print the result
print(gap_stat, method = "firstmax")

# Alternative using the fviz_gap_stat function
fviz_gap_stat(gap_stat)


########## 5 clusters
cluster5<-kmeans(data,centers=5,nstart=25)

# clusytering results
str(cluster5)
cluster5

#visualization  of clusters 
fviz_cluster(cluster5, data=data,  choose.vars = c("Grocery", "Frozen"))


############ Prediction
clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

# new data to predict
x_new <- data

colnames(x_new) <- colnames(data)

all.equal(cluster5[["cluster"]], clusters(x_new, cluster5[["centers"]]))

clusters(x_new, cluster5[["centers"]])


#unscaling centers


cluster5$centers

unscaled_Centers<- unscale(cluster5$centers, data)
  
unscaled_Centers[,1:2]<- round(unscaled_Centers[,1:2])

unscaled_Centers
