# Arboles de regresion de prediccion de casas

# install.packages("rpart", "rpart.plot", "caret") 
library(rpart)      # Arboles
library(rpart.plot) # Visualizar y represenar árboles
library(caret)      # Para llevar a cabo particiones de conjuntos de datos en caso de...
library(dplyr)      # Para select, filter, mutate, arange ....
library(readr)      # Para leer datos
library(ggplot2)    # Para grafica mas vistosas
library(reshape)    # Para renombrar columnas


# CARGAR DATOS ******
# Los datos de entrenamiento
entrena <- read_csv("datos/house-prices-advanced-regression-techniques/train.csv")

# Los datos de validación
valida <- read.csv("datos/house-prices-advanced-regression-techniques/test.csv")


# DEPURAR Y LIMPIAR DATOS
# Detectar cuáles variables a utilizar como independientes
# y que sólo son variables numéricas
sapply(entrena, is.numeric) # Cuáles son ?
cualesNumericas <- which(sapply(entrena, is.numeric))
nombresColumnas <- names(cualesNumericas)
nombresColumnas

# Conjunto de datos de sólo numéricas
entrenaNumericas <- select(entrena, nombresColumnas)
entrenaNumericas

# Renombrar variabes que están con nombres `1...`, `2...`, `3...`

entrenaNumericas = rename(entrenaNumericas, c(`1stFlrSF`="lstFlrSF"))  # opcion 2, renombra la variable
entrenaNumericas = rename(entrenaNumericas, c(`2ndFlrSF`="tndFlrSF"))  # renombra la variable
entrenaNumericas = rename(entrenaNumericas, c(`3SsnPorch`="t3SsnPorch"))  # renombra la variable

entrenaNumericas

# Solo las variables numéricas


# Cuantos registros?
n = nrow(entrenaNumericas)
n

# Contruir el MODELO árbol
# El comando para generar un modelo de árbol de decisión, usando la librería rpart 
# lleva el mismo nombre


set.seed(2020) # Semilla

arbol <- rpart(formula = SalePrice ~ ., data = entrenaNumericas)
arbol


# Vamos a verlo gráficamente rpart.
# extra	
# Display extra information at the nodes. Possible values: "auto" (case insensitive) Default.
# Automatically select a value based on the model type, as follows:
# extra=106 class model with a binary response
# extra=104 class model with a response having more than two levels
# extra=100 other models
# rpart.plot(arbol, extra = 106)   # Pendiente .... No funciona

prp(arbol, type = 2, nn = TRUE, 
    fallen.leaves = TRUE, faclen = 4,
    varlen = 8,  shadow.col = "gray")


# ctable

arbol$cptable


# plotcp(arbol)
plotcp(arbol)

# Podar el árbol prune()
# MAs simple que el primer arbol
arbol.Recortado <- prune(arbol, cp = 0.01989959)

prp(arbol.Recortado, type = 2, nn = TRUE, 
    fallen.leaves = TRUE, faclen = 4,
    varlen = 8,  shadow.col = "gray")





