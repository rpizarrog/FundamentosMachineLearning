# Verificar en que directorio estamos
# cargar los datos de coronavirus
# Ver cuantos registros tenemos
# summary(datos)

#print ("Hola Mundo, de nuevo")

# Cargar las librerías
library(readr)
library(dplyr)
library(ggplot2)

print ("Cargar Datos")

cat("probando un nuevo print")



# Cargar los datos
datos <- read.csv("datos/covid_19_data.csv")
datos

# Se pueden cargar datos desde gihub
datos <- read.csc("https://raw.githubusercontent.com/rpizarrog/FundamentosMachineLearning/master/datos/covid_19_data.csv")
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

class(datos)
head(datos)
tail(datos)
nrow(datos)
ncol(datos)


casos <- datos %>%
          group_by(Country.Region) %>%
          summarise(casosConfirmados = sum(Confirmed),
                    casosDescesos = sum(Deaths))

casos <- data.frame(arrange(casos, desc(casosConfirmados) ))

casos <- cbind(casos, porc=paste(round(casos$casosDescesos / casos$casosConfirmados * 100,2),'%'))

casos




