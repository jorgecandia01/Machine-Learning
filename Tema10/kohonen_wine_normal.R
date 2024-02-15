library(kohonen) # for building the SOM map
library(caret)     #for confusion matrix

# Loading data from the kohonen package
data(wines)

#Scaling original data
set.seed(7)
wines.sc<-scale(wines)

# creation of training and test datasets
index <- sample(nrow(wines.sc), round(0.75*nrow(wines.sc)))
train <- wines.sc[index,] 
test <- wines.sc[-index,]

train_label<-vintages[index]
test_label<-vintages[-index]

#main characteristics of the map
som_grid<-somgrid(xdim=5, ydim=4, topo="hexagonal")

#training the map
wine.som <- som(train, grid=som_grid , 
               rlen=100, alpha=c(0.05, 0.01), 
               radius= 2, keep.data=T)

# Names of the variables used (Wine type is not there)
colnames(train)

# main characteristics of the map
summary(wine.som)

# weights of the map or patterns obtained
wine.som$codes

#Neuron where is training sample belongs
wine.som$unit.classif

#Showing the training process
plot(wine.som, type="changes")

#node counts
plot(wine.som, type="counts", main=" number of examples per neuron")
identify(wine.som) #cliking you can see the value after ESC

#Neighbour distances
plot(wine.som, type="dist.neighbours", main="SOM neighbour distance")
identify(wine.som) #cliking you can see the value after ESC

#Codes/Weight vectors
plot(wine.som, type="codes", main="patterns discovered")

#plotting quality
plot(wine.som, type="quality", main="Node Quality/Distance")

#plotting mapping of data
plot(wine.som, type="mapping", main="Data mapping to the neurons")

#heat maps
coolBlueHOtRed<-function(n, alpha=1){rainbow(n,end=4/6, 
                                             alpha=alpha)[n:1]}

par(mfrow=c(3,2))
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,1],
     main=colnames(getCodes(wine.som, 1))[1],
     palette.name=coolBlueHOtRed)
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,2],
     main=colnames(getCodes(wine.som, 1))[2],
     palette.name=coolBlueHOtRed)
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,3],
     main=colnames(getCodes(wine.som, 1))[3],
     palette.name=coolBlueHOtRed)
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,4],
     main=colnames(getCodes(wine.som, 1))[4],
     palette.name=coolBlueHOtRed)
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,5],
     main=colnames(getCodes(wine.som, 1))[5],
     palette.name=coolBlueHOtRed)
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,6],
     main=colnames(getCodes(wine.som, 1))[6],
     palette.name=coolBlueHOtRed)
########### More heatmaps
par(mfrow=c(3,2))
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,7],
     main=colnames(getCodes(wine.som, 1))[7],
     palette.name=coolBlueHOtRed)
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,8],
     main=colnames(getCodes(wine.som, 1))[8],
     palette.name=coolBlueHOtRed)
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,9],
     main=colnames(getCodes(wine.som, 1))[9],
     palette.name=coolBlueHOtRed)
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,10],
     main=colnames(getCodes(wine.som, 1))[10],
     palette.name=coolBlueHOtRed)
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,11],
     main=colnames(getCodes(wine.som, 1))[11],
     palette.name=coolBlueHOtRed)
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,12],
     main=colnames(getCodes(wine.som, 1))[12],
     palette.name=coolBlueHOtRed)
#Last heatmap
par(mfrow=c(2,2))
plot(wine.som, type="property", property=getCodes(wine.som, 1)[,13],
     main=colnames(getCodes(wine.som, 1))[13],
     palette.name=coolBlueHOtRed)


# Alternative easier
coolBlueHotRed<-function(n, alpha=1){rainbow(n,end=4/6, 
                                             alpha=alpha)[n:1]}

par(mfrow=c(5,3))
for (j in 1:ncol(train)) {
plot(wine.som, type="property", property=wine.som$codes[[1]][,j],
     palette.name=coolBlueHotRed,
     main=colnames(train)[j], cex=0.5)
}

#Clustering patterns in the map
groups<-3
#Applying hierarchical clustering for grouping patterns
wine.hc=cutree(hclust(dist(wine.som$codes[[1]])), groups)
plot(wine.som, type="codes", bgcol = rainbow(groups)[wine.hc],
     main="clustering the patterns discovered")
add.cluster.boundaries(wine.som,wine.hc)



################################
####### SOM SUPERVISED 
set.seed(7)
kohmap <- xyf(train, train_label,
              grid=som_grid, 
                rlen=100, alpha=c(0.05, 0.01), 
                radius= 2, keep.data=T)

#Showing the training process
plot(kohmap, type="changes")

#Showing distribution of wine labels in neurons
par(mfrow=c(1,2))
plot(kohmap, type="codes",  codeRendering = "lines", shape="straight",
     main=c(" patterns", "Wine types"))


# Plotting cases in neurons
plot(kohmap, type="mapping", labels=as.numeric(train_label),
     col=as.numeric(train_label)+1, pch=1, main="Map of classes")


#Prediction using the training data
kohmap.predict_tr<- predict(kohmap, newdata=train, whatmap = 1)

prediction_table_tr<-table(train_label,kohmap.predict_tr$predictions[[2]])

confusionMatrix(prediction_table_tr)

#Prediction using the test data
kohmap.predict<- predict(kohmap, newdata=test, whatmap = 1)

prediction_table<-table(test_label,kohmap.predict$predictions[[2]])

confusionMatrix(prediction_table)

