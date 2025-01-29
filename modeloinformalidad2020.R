installed.packages()
install.packages("readxl")
library(readxl)
file.choose()
ruta_excel<-"C:\\Users\\Lenovo\\Desktop\\datas\\datostesis2020.xlsx"
excel_sheets(ruta_excel)
datos2020<-read_excel(ruta_excel)
str(datos2020)
library(nnet)
#Tabla de frecuencia de total de mujeres y hombres en la informalidad#
Frecuencia2020 <- xtabs(~ Informal + genero, data = datos2020)
Frecuencia20201 <- cbind(Frecuencia2020, apply(Frecuencia2020,1,sum))
Frecuencia20202<- rbind(Frecuencia2020, apply(Frecuencia2020,2,sum))
rownames(Frecuencia2020)[3] <- c('Total')
## Binomial##
modelo2020<- glm(Informal~ genero + jdeh + edad + escolaridad + Ingreso + Pobre, family=binomial(), data=datos2020)
coef(modelo2020)
summary(modelo2020)
exp(modelo3$coefficients)
## AIC para el modelo2020 ##
step(modelo2020, direction = "backward")
install.packages("ROCR")
library(ROCR)
pred2020<-predict(modelo2020, type="response")
clas<-ifelse(pred2020 > 0.5, "1", "0")
mean(clas == datos2020$Informal)
##El 78.36% de las veces el modelo predijo los datos reales, por lo tanto hay un 21.64% de error en el modelo##
##En que tipo de informalidad están más las mujeres, en la 3 Empleada domestica color verde y trabajador familir sin remuneración##
tc2020<-table(datos2020$clftrabajo,as.factor(datos2020$genero))
mosaicplot(tc2020, color = c("red","lightblue"))
##Informalidad de acuerdo al género##
tc22020<-table(datos2020$Informal,as.factor(datos2020$genero))
mosaicplot(tc22020, color = c("#d8001d","#77FF00"))

tc220200<-table(datos2020$clftrabajo,as.factor(datos2020$Informal))
mosaicplot(tc220200, color = c("#d8001d","#77FF00"))
##Informalidad y pobreza##
plot(x=datos2020$Ingreso, y=datos2020$Informal)
#histograma escolaridad#
install.packages("ggplot2")
library(ggplot2)
ggplot(datos2020, aes(x = escolaridad, y = Ingreso, color = Informal)) + geom_point()
hist(datos2020$escolaridad)
datos_hombres21<- subset(datos2020, genero == "hombre")
datos_mujeres21 <- subset(datos2020, genero == "mujer")
hist(datos_mujeres21$escolaridad, breaks=25, col = "pink")
hist(datos_hombres21$escolaridad, breaks=50, col = "blue", add=TRUE, main = "Histograma mujeres y hombres")
#Histograma edad#

hist(datos2020$edad)
#Histograma ingresos#
datos_filtrados2020 <- datos2020[datos2020$Ingreso <= 6000000,]
hist(datos_filtrados2020$Ingreso, breaks=100, col = "blue")
# Filtrar datos para hombres y mujeres
datos_hombres2020 <- subset(datos2020[datos2020$Ingreso <= 6000000,], genero == "hombre")
datos_mujeres2020 <- subset(datos2020[datos2020$Ingreso <= 6000000,], genero == "mujer")

# Crea la nueva variable y asigna NA a todos los valores por defecto
datos2020$Ingreso2020 <- NA
datos2020$Ingresopobres2020 <- NA
datos2020$Ingresonopobres2020 <- NA
Ingreso20 <- datos2020$Ingreso
# Filtra los valores de 'variable_original' menores a 600 y asígnalos a 'nueva_variable'
datos2020$Ingreso2020[datos2020$Ingreso <= 6000000] <- datos2020$Ingreso[datos2020$Ingreso <= 6000000]
datos2020$Ingresopobres2020[datos2020$Ingreso <= 327674] <- datos2020$Ingreso[datos2020$Ingreso <= 327674]
datos2020$Ingresonopobres2020[datos2020$Ingreso < 2000000 & datos2020$Ingreso > 327674] <- datos2020$Ingreso[datos2020$Ingreso > 327674]

# Crear un histograma de ingresos para hombres y mujeres
library(ggplot2)
hist(datos_hombres2020$Ingreso, breaks=45, col = "blue")
hist(datos_mujeres2020$Ingreso, breaks=50, col = "pink", add=TRUE, main = "Histograma mujeres y hombres")

#Regresion escolaridad e informalidad 2020# Modelo de regresion logistica 1 ajustado#
modelo20201<- glm(Informal~ genero + edad + escolaridad + Ingreso + Pobre , family=binomial(), data=datos2020)
pred20201<-predict(modelo20201, type="response")
library(ggplot2)
ggplot(datos2020, aes(x = escolaridad , y = pred20201, color = genero)) + geom_point()
g2020 <- ggplot(datos2020, aes(x=escolaridad, y=pred20201, color=genero))
g2020 + geom_point() + geom_smooth(method="lm")

ggplot(datos2020, aes(x = escolaridad , y = pred20201, color = genero)) + geom_point()
g20201 <- ggplot(datos2020, aes(x=escolaridad, y=pred20201, color=genero))
g20201 + geom_point() + geom_smooth(method="lm")

#Pintar modelo##Regresion Ingreso e informalidad#
library(ggplot2)
ggplot(datos2020, aes(x = Ingreso2020 , y = pred20201, color = genero)) + geom_point()

#Regresion Ingreso personas pobres e informalidad#
ggplot(datos2020, aes(x = Ingresopobres2020 , y = pred20201, color = genero)) + geom_point()
g2m <- ggplot(datos2020, aes(x=Ingresopobres2020, y=pred20201, color=genero))
g2m + geom_point() + geom_smooth(method="lm")

#Regresion Ingreso personas no pobres e informalidad#
ggplot(datos2020, aes(x = Ingresonopobres2020 , y = pred2020, color = genero)) + geom_point()
g3 <- ggplot(datos2020, aes(x=Ingresonopobres2020, y=pred2020, color=genero))
g3 + geom_point() + geom_smooth(method="lm")


#box plot#
library(ggplot2)
g4m <- ggplot(datos2020, aes(x=Ingreso20 , y=pred20201, fill=genero))
g4m + geom_boxplot(width=0.4, colour="gray60", outlier.colour = "red")

#otra1
modelo20202<- glm(Informal~ genero + edad + escolaridad + Ingreso + Pobre, family=binomial(), data=datos_mujeres2020)
g5m <- ggplot(datos2020, aes(x=escolaridad, y=predict(modelo20202, type="response"), color=genero))
g5m + geom_point() + geom_smooth(method="lm")



#Para guardar
setwd("C:/Users/Lenovo/Desktop/datas")
write.table(pred,"pred_x.csv", sep = ";")
write.table(modelo3,"modelo3.csv", sep = ";")
