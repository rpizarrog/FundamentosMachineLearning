# Prediccion de casas


# LIBRERIAS
library(readr)
library(dplyr)

# CARGAR DATOS ******
# Los datos de entrenamiento
entrena <- read_csv("datos/house-prices-advanced-regression-techniques/train.csv")

# Los datos de validación
valida <- read.csv("datos/house-prices-advanced-regression-techniques/train.csv")

# Ver los rimeros regisros de cada conjunto de datos
head(entrena)
head(valida)

# EXPLORAR ******
# Explorar datos de entrenamiento
summary(entrena)
# Conocer etructura de datos entrenamient str()
str(entrena)
nrow(entrena)
ncol(entrena)

# EXPLORAR datos
# Se visualiza como una distribución normal
ggplot(entrena, aes(x=SalePrice)) +
  geom_histogram()


# DEPURAR ******
# Cómo detectar cuáles variables utilziar como independientes
# Sólo las variables numéricas

sapply(entrena, is.numeric) # Cuáles son ?
cualesNumericas <- which(sapply(entrena, is.numeric))
nombresColumnas <- names(cualesNumericas)
nombresColumnas
# Ver el diccionario de datos ???

# Conjunto 
entrenaNumericas <- select(entrena, nombresColumnas)
entrenaNumericas

# CORRELACION *****
correlaciones <- data.frame(cor(entrenaNumericas))
correlaciones

cor.soloPrecio <- select (correlaciones, SalePrice) %>%
  filter(SalePrice >= 0.6)
cor.soloPrecio

# Lo vemos en View
View(correlaciones)
# Las columnas mas correlaciones con SalePrice
# OverallQual, GrLivArea, GarageCars, GarageArea, TotalBsmtSF, 1stFlrSF

# plot

plot(x = entrena$OverallQual, y=entrena$SalePrice)

# Con ggplot

ggplot(entrena, aes(x=OverallQual, y=SalePrice)) +
  geom_point(color="darkred") + geom_smooth(method = "lm")

ggplot(entrena, aes(x=GrLivArea, y=SalePrice)) +
  geom_point(color="darkgreen") + geom_smooth(method = "lm")

ggplot(entrena, aes(x=GarageCars, y=SalePrice)) +
  geom_point(color="yellow") + geom_smooth(method = "lm")







# Cuáles son variables numéricas

