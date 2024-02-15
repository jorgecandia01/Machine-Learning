library(openxlsx)

data2 <- read.xlsx("datasetalquilereschamberi2.xlsx", sheet = 1)
data3 <- read.xlsx("datasetalquilereschamberi3.xlsx", sheet = 1)


#2
for (i in (1:6)){
  row <- data2[i]
  if(anyNA(row)){
    print(paste(colnames(data2)[i], '-> YES NA'))
  } else {
    print(paste(colnames(data2)[i], '-> NO NA'))
  }
}


#3
x <- c()
for(i in 1:nrow(data2))
{
  if(anyNA(data2[i,]))
  {
    print(i)
    x <- c(x,i)
  }
}

new_data2 <- data2[-c(x),]



#4
pm2 <- data3$Precio / data3$Superficie
summary(pm2)
boxplot(pm2, horizontal=TRUE)
x <- sort(pm2)
print(x[64]) #cuartil 40



#5
pm2 <- data3$Precio / data3$Superficie
new_data3 <- data3
new_data3$pm2 <- pm2

maxPM2 <- new_data3[new_data3$pm2 %in% max(new_data3$pm2),]
minPM2 <- new_data3[new_data3$pm2 %in% min(new_data3$pm2),]



#6
dsol <- data3$Distanciasol
hist(dsol)
boxplot(dsol, horizontal=TRUE)
summary(dsol)



#7
Megusta <- c()

for(i in 1:nrow(data3))
{
  row <- data3[i,]
  if(row$Distanciasol<3 | row$Precio<1000)
  {
    Megusta <- c(Megusta,"SI")
  }
  else
  {
    Megusta <- c(Megusta,"NO")
  }
}

nSIMegusta <- Megusta[Megusta == "SI"]
print(160 - length(nSIMegusta))



#8
ntotal <- nrow(data3) #Pilla el nº de rows de data3

t <- sample(1:ntotal, 0.8*ntotal) #Elige una muestra aleatoria del 80% de las filas
#t es un vector con 128 números seleccionados aleatoriamente de un conjunto de nº enteros del 1 a 160

training <- data3[t,] #Extrae la muestra de data3 (antes solo eligió los numeros de fila)

validacion <- data3[-t,] #Extrae todas las filas menos las de la muestra



#9
gmean <- function(v){
  medias<-vector(mode="numeric", length(v))
  for(i in 1:length(v)){
    medias[i] <- mean(v[1:i])
  }
  medias
}

print(gmean(c(1,2,3,4,5,6,7,8)))



#10
table(data3$Altura)

unique(data3$Altura)

data3[order(data3$Precio),] #IMP

sort(data3$Precio)








