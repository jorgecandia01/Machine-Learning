
library(ggplot2) # Required for plotting with ggplot
library(e1071) #Required for SVM
library(vcd) # For mosaic

#Data download
load("ESL.mixture.rda")

data<-data.frame(ESL.mixture$x,y=ESL.mixture$y)
# Conversion of y variable to factor
data$y<-as.factor(data$y)
head(data)

ggplot(data=data,aes(x=X1, y=X2, color=y))+
  geom_point(size=2.5)+
  theme_bw()+theme(legend.position ="none")

#Creation of a simple Support Vector Classifier 

# Creating the SVC model. Use kernel as "radial"
model_svc<-svm(formula=y ~ X1+X2, data=data, kernel="radial", 
               cost=10, scale=FALSE)

summary(model_svc)

#Support vectors
model_svc$index 

#Plotting the two regions in low resolution
plot(model_svc, data)


#Evaluation of the optimum  hyperparameters 
set.seed(1)
svc_cv<-tune("svm", y ~ X1+X2, data=data, kernel="radial",
             ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 20), 
                               gamma=c(0.5, 1, 2, 3, 4, 5, 10)))

summary(svc_cv)

# Plotting error versus cost
ggplot(data=svc_cv$performances, aes(x=cost, y=error, 
                                     color=factor(gamma)))+
  geom_line()+
  geom_point()+
  labs(title="Classification error vs  C & gamma")+
  theme_bw()+theme(legend.position ="bottom")

svc_cv$best.parameters

#Saving the best model found for C and gamma
best_model <- svc_cv$best.model

#Prediction of new observations

# New data are obtained interpolating points in the range of
# both predictors X1 and X2.
# The new points are used for predicting the output of the model 
# coloring the regions that separate the hyperplane


# Range of predictors
range_X1 <- range(data$X1)
range_X2 <- range(data$X2)

# Interpolation of points
new_x1 <- seq(from = range_X1[1], to = range_X1[2], length = 75)
new_x2 <- seq(from = range_X2[1], to = range_X2[2], length = 75)

new_points <- expand.grid(X1 = new_x1, X2 = new_x2)

# Prediction using the new points according to the model
predictions <- predict(object = best_model, newdata = new_points)


# Storing the predictions in a dataframe for the color

color_regions <- data.frame(new_points, y = predictions)

ggplot() +
  # Representation of the 2 regions using the points and colouring
  # them according to the class predicted by the model
  geom_point(data = color_regions, aes(x = X1, y = X2, 
                                color = as.factor(y)), size = 0.5) +
  # The observations are added
  geom_point(data = data, aes(x = X1, y = X2, color = as.factor(y)), 
                      size = 2.5) +
  # Identification of observations thar are suppoer vectors
  geom_point(data = data[best_model$index, ], aes(x = X1, y = X2,
          color = as.factor(y)), shape = 21, colour = "black", 
          size = 2.5) +
          theme_bw() + theme(legend.position = "none")
             
             

 
####### table of linnear case is not possible because we do not
#       know the real value for y

