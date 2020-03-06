# Verificar ¿en qué directorio se encentran?
# cargar los datos de coronavirus
# Ver ¿cuántos registros se tienen?
# summary(datos)

# Cargar las librerías
library(readr)
library(dplyr)
library(ggplot2)

# Cargar los datos
datos <- read.csv("datos/covid_19_data.csv")
datos

# Se pueden cargar datos desde gihub
datos <- read.csc("https://raw.githubusercontent.com/rpizarrog/FundamentosMachineLearning/master/datos/covid_19_data.csv")
datos


# Explorar los datos
summary(datos)
str(datos)

unique(datos$Country.Region)
class(datos)
head(datos)
tail(datos)
nrow(datos)
ncol(datos)



# ¿cuántos casos confirmados ?
sum(datos$Confirmed)

# ¿cuántos desscesos?
sum(datos$Deaths)

# ¿porcentaje, decesos confirmados
sum(datos$Confirmed / sum(datos$Deaths))

# Porcentaje
paste(round(sum(datos$Confirmed / sum(datos$Deaths)),2), " % ")


# Resumir por País y Región
casos <- datos %>%
          group_by(Country.Region) %>%
          summarise(casosConfirmados = sum(Confirmed),
                    casosDecesos = sum(Deaths))

# Ordenar descendente y agregar columna porc =  %
casos <- data.frame(arrange(casos, desc(casosConfirmados) ))

casos <- cbind(casos, porc=paste(round(casos$casosDecesos / casos$casosConfirmados * 100,2),'%'))

casos

# Interpretación
# Llama la atención que en países como Irán y Filipinas hay un alto porcentaje de decesos
# en relación a los casos detectados
# Por supuesto, en China hay mas casos confirmados y decesos



