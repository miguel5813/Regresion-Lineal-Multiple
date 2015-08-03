library(readxl)
#EJERCICIO 2
## 2.1
poblacion1 <- read_excel("poblacion1.xlsx", sheet=1, na=" ")
poblacion2 <- read_excel("poblacion2.xlsx", sheet=1, na=" ")
dim1<-dim(poblacion1)
dim2<-dim(poblacion2)
dim1
poblacion1
poblacion2

## 2.2.1
poblacion<-merge(poblacion1, poblacion2, by="identificador",suffixes=c("",""))
View (poblacion)

## 2.2.2
for(j in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,j])==TRUE){
    boxplot(poblacion[,j], main = "Diagrama de Cajas", col = "green")
  }else {
    barplot(table(poblacion[,j]), col = "blue", main = "Diagrama de Barras")
  }
}

## 2.3
  for(j in 2:dim(poblacion)[2]){
    if(is.numeric(poblacion[,j])==TRUE){
      
      print(names(poblacion)[j])
      print("minimo=")
      print(min(poblacion[,j]))
      print("media=")
      print(mean(poblacion[,j]))
      print("maximo=")
      print(max(poblacion[,j]))
      print("desviacion estandar=")
      print(sd(poblacion[,j]))
      print("cuartiles=") 
      print(quantile(poblacion[,j],probs=seq(0,1,0.25),na.rm = FALSE))
    }else {
      
      print(names(poblacion)[j])
      print("frecuencia")
      print(table(poblacion[,j])/dim(poblacion)[1])

    }
    
  }

## 2.4
for(i in 3:dim(poblacion)[2]){
  if(is.numeric(poblacion[,i])==TRUE){
    correlacion<-cor(poblacion[,2],poblacion[,i])
    print(correlacion)
  }
}

## 2.5
t.test(poblacion ~ serv.bas.compl , data = poblacion, conf.level=0.9)

## 2.6
regresion<-lm(poblacion~var.pobl.mayor+menores.18+tasa.crimen,data=poblacion)
summary(regresion)

## 2.7
summary(regresion)["r.squared"]

## 2.8
summary(aov(regresion))

## 2.9
residuo <- regresion[["residuals"]]
prediccion <- regresion[["fitted.values"]]
data <- data.frame("poblacion", prediccion,residuo)
hist(residuo,15)
mean(residuo)
qqnorm(residuo)
qqline(residuo,col="red")
plot(residuo,prediccion)

