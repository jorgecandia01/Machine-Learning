
# 1. Let us begin with the basics. Create a vector with values 2002, 2004, 2006, 2008 using c
# and seq functions.

v1 <- c(2002, 2004, 2006, 2008)
v2 <- seq(2002, 2008, by=2)

v <- v1


# 2. Use function length to get the size of the vector.

length(v)


# 3. Try the different methods for selecting the elements of the vector.

v[1]
v[0:2]
v[-3]


# 4. Load the data set usedcars.csv into a variable named fdata.

fdata <- read.table(file = "usedcars.csv",
                    sep = ',',
                    header = TRUE,
                    na.strings = 'NA',
                    stringsAsFactors = FALSE)


# 5. Use str and summary functions on fdata. What types of variables are in the dataset?
# What are the average values of the numeric variables?

str(fdata)
summary(fdata)


# 6. Use View and head functions on fdata.

View(fdata) #Ojo V mayuscula
head(fdata)


# 7. Access the elements number 5 to 20 of variable color.

color <- fdata$color
color5_20 <- color[5:20]


# 8. Create a new dataset removing row numbers 10 and 100.

fdata2 <- fdata[-(10:100),]
View(fdata2)

##PARA SOLO QUITAR LAS FILAS 10 Y 100, PONGO -c ==> fdata2 <- fdata[-c(10,100),]
fdata22 <- fdata[-c(10,100),]

# 9. Create a new dataset only with columns year, price and mileage.

fdata3 <- fdata[,c(1,3,4)] #HAY MANERA DE PONER LOS NOMBRES DE LAS COL EN VEZ DE  NUMS? SI
View(fdata3)

fdata33 <- fdata[,c('year','price','mileage')]
View(fdata33)



# 10. Obtain statistics for variables year and price.

###YEAR:

year <- fdata[[1]]

yearMax <- max(year)
yearMin <- min(year)
yearMean<- mean(year)
yearMed <- median(year)
yearQnt <- quantile(year)
yearSd <- sd(year)

summary(fdata$price) 

##PRICE

price <- fdata$price

priceMax <- max(price)
priceMin <- min(price)
priceMean<- mean(price)
priceMed <- median(price)
priceQnt <- quantile(price)
priceSd  <- sd(price)

  

# 11. Use function by() to calculate statistics filtering by classes.

by(fdata$price, fdata$transmission, mean) #IMP
by(fdata$year, fdata$transmission, mean)

a <- by(fdata$price, fdata$color, mean)
max(a)


# 12. Filter from this dataset the rows that have a year that matches the values of the vector
# created in step 1.

rows <- fdata[fdata$year %in% v1,] #IMP
View(rows)


# 13. Create a new column in the dataset named PM resulting from multiplying the values of
# price and mileage in each row.

PM <- fdata$price * fdata$mileage
fdata4 <- fdata
fdata4$PM <- PM


# 14. Plot the price values with a solid line.

plot(fdata$price, type='l', main='Price')


# 15. Plot a scatterplot between variables mileage (x axis) and price (y axis).

plot(fdata$mileage, fdata$price)


# 16. Plot a boxplot of mileage values.

boxplot(fdata$mileage, horizontal=TRUE)


# 17. Plot a histogram of the prices data.

hist(fdata$price)





