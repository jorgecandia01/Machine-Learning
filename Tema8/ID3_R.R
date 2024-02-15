# ID3 Classification using data.tree
# Author Christoph Glur

library(data.tree) #Required for hierarchical organization
data(mushroom) #Loading simple example

tble <- table(mushroom[,c('color', 'edibility')])
tble


# Definition of purity
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

#Definition of entropy
Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

Entropy(c(10, 0))
Entropy(c(0, 7))
Entropy(c(3, 7))


#Information Gain
InformationGain <- function( tble ) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, 
                                          MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

#Testing examples
InformationGain(tble)
InformationGain(table(mushroom[,c('size', 'edibility')]))
InformationGain(table(mushroom[,c('points', 'edibility')]))


# ID3 code
TrainID3 <- function(node, data) {
  
  node$obsCount <- nrow(data)
  
  #if the data-set is pure (e.g. all toxic), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    #chose the feature with the highest information gain (e.g. 'color')
    ig <- sapply(colnames(data)[-ncol(data)], 
                 function(x) InformationGain(
                   table(data[,x], data[,ncol(data)])
                 )
    )
    feature <- names(ig)[ig == max(ig)][1]
    
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    childObs <- split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }
    
  }
  
  
  
}

#Using the ID3 function
tree <- Node$new("mushroom")
TrainID3(tree, mushroom)
print(tree, "feature", "obsCount")

# Prediction function
Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}

# Using prediction
Predict(tree, c(color = 'red', 
                size = 'large', 
                points = 'yes')
)


