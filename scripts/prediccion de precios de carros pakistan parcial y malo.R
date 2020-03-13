# Las librerías
library(rpart)      # Arboles
library(rpart.plot) # Visualizar y represenar árboles
library(caret)      # Para llevar a cabo particiones de conjuntos de datos en caso de...
library(dplyr)      # Para select, filter, mutate, arange ....
library(readr)      # Para leer datos
library(ggplot2)    # Para grafica mas vistosas
library(reshape)    # Para renombrar columnas


# Analizar y aplicar regresión de datos del precio de los autos
datos <- read_csv("datos/OLX_Car_Data_CSV.csv/OLX_Car_Data_CSV.csv")

# Cambiar nombres de columnas mas prácticas
datos = rename(datos, c("Registered City" ="RegCity"))
datos = rename(datos, c("Transaction Type" ="TransType"))
datos = rename(datos, c("KMs Driven" ="KilDriven"))


# Cambiar  factor
datos$Brand <- factor(datos$Brand)
datos$Fuel <- factor(datos$Fuel)
datos$Condition <- factor(datos$Condition)
datos$Model <- factor(datos$Model)
datos$RegCity <- factor(datos$RegCity)
datos$TransType <- factor(datos$TransType)


set.seed(2020) # Semilla
entrena <- createDataPartition(datos$Price, p=0.7, list = FALSE)
head(entrena)

datos.Entrena <- datos[entrena,]
head(datos.Entrena)

datos.Valida <- datos[-entrena,]
head(datos.Valida)

# Histograma de Price
hist(x = datos$Price)

modelo <- lm(Price ~ Brand + Condition  + KilDriven +
               Fuel + RegCity + TransType + Year, data = datos.Entrena)

modelo$coefficients

summary(modelo)

# No es adecuado el modelo de regresión lineal para estos datos

# ARBOLES DE REGRESION

arbol <- rpart(formula = Price  ~ ., data = datos.Entrena)
arbol

# VEMOS EL ARBOL
prp(arbol, type = 2, nn = TRUE, 
    fallen.leaves = TRUE, faclen = 4,
    varlen = 8,  shadow.col = "gray")

# Tabla
arbol$cptable

# En que rama podamos?
plotcp(arbol)

# Podar en CI = 0.01480645
arbol.Recortado <- prune(arbol, cp = 0.01480645)

arbol.Recortado

prp(arbol.Recortado, type = 2, nn = TRUE, 
    fallen.leaves = TRUE, faclen = 6,
    varlen = 6,  shadow.col = "gray")
