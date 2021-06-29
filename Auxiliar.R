#Se cargan las librerías que se van a usar:

library(readxl)
library(xlsx)
library(ggplot2)
library(forecast)
library(MASS)
library(ggfortify)
library(tseries)
library(dplyr)
library(parsnip)
library(rsample)
library(timetk)
library(modeltime)
library(randomForest)
library(astsa)
library(lmtest)
library(tune)
library(workflows)
library(yardstick)
library(recipes)
library(bsts)
library(dials)

#Definimos la función para extraer los datos:
ext<-function(dataset,ruta){#ruta tiene que ir entre comillas
  ruta_tot<-as.data.frame(select(dataset,ruta,Fecha))
  ruta_train<-subset(ruta_tot,Fecha<="2020-08-01")
  ruta_test <- subset(ruta_tot,Fecha >"2020-08-01")
  return(list(ruta_tot,ruta_train,ruta_test))
}

Datos <- read_excel("Datos TFG.xlsx", sheet="Mensual")

