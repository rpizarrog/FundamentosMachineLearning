# Verificar en que directorio estamos
# cargar los datos de coronavirus
# Ver cuantos registros tenemos
# summary(datos)

print ("Hola Mundodfgdfg ")

# Cargar las librerías
library(readr)
library(dplyr)

print ("Cargar Datos")

cat("probando un nuevo print")



# Cargar los datos
datos <- read.csv("datos/covid_19_data.csv")
datos

summary(datos)
str(datos)

unique(datos$Country.Region)


# ¿Cuángos casos confirmados ?
sum(datos$Confirmed)

# ¿cántos desscesos?
sum(datos$Deaths)

# ¿Porcentaje, descesos confirmados
sum(datos$Confirmed / sum(datos$Deaths))

paste(round(sum(datos$Confirmed / sum(datos$Deaths)),2), " % ")









