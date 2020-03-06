# Regresión mútiple con datos de melbourne
library(readr)
library(dplyr)
library(ggplot2)
library(reshape) # Para renombrar columnas
library(caret) # Para particiones
library(corrplot)

getwd()
datos <- read.csv("datos/melb_data.csv")
head(datos)
tail(datos)

# Crear conjunto de datos entrenamiento y valdiación
set.seed(2020) # Semilla
entrena <- createDataPartition(datos$Price, p=0.7, list = FALSE)
head(entrena)
nrow(entrena)

# Los registros que no estén en entrena
head(datos[-entrena,])
nrow(datos[-entrena,])


# Ver los primeros seis datos con sólo variables numéricas
head(datos)

# Ahora a determinar conjuntos de datos de entrenamiento y luego head()
datos.Entrena <- datos[entrena,]
head(datos.Entrena)
summary(datos.Entrena)

# y conjunto de datos de validación y luego head()
datos.Valida <- datos[-entrena,]
head(datos.Valida)
summary(datos.Valida)


# Asegurarse que se tienen los datos categóricos character 
# como tipo factor
# [1] "Suburb"        "Address"       "Rooms"        
# [4] "Type"          "Price"         "Method"       
# [7] "SellerG"       "Date"          "Distance"     
# [10] "Postcode"      "Bedroom2"      "Bathroom"     
# [13] "Car"           "Landsize"      "BuildingArea" 
# [16] "YearBuilt"     "CouncilArea"   "Lattitude"    
# [19] "Longtitude"    "Regionname"    "Propertycount"

# Seleccionar ciertas variables de interés
datos.Entrena <- select(datos.Entrena, -Suburb,-Address,-SellerG,-Date,-Postcode,-CouncilArea, -Lattitude, -Longtitude,-Regionname )
modelo <- lm(data = datos.Entrena, formula = Price ~ .)
modelo


# Interpretación
