# Prediccion de casas


# LIBRERIAS
library(readr)
library(dplyr)

# CARGAR DATOS ******
# Los datos de entrenamiento
entrena <- read_csv("datos/house-prices-advanced-regression-techniques/train.csv")

# Los datos de validación
valida <- read.csv("datos/house-prices-advanced-regression-techniques/test.csv")

# Ver los primeros regisros de cada conjunto de datos
head(entrena)
head(valida)

# EXPLORAR ******
# Explorar datos de entrenamiento
summary(entrena)
# Conocer etructura de datos entrenamient str()
str(entrena)
nrow(entrena)
ncol(entrena)

# EXPLORAR datos de entrenamiento
# Se visualiza como una distribución normal
ggplot(entrena, aes(x=SalePrice)) +
  geom_histogram()


# DEPURAR ******
# Cómo detectar cuáles variables utilzar como independientes
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
cor.soloPrecientrenaCol.Correl.Priceo

# Otra manera de saber las variables
entrenaCol.Correl.Price <- data.frame(cbind(rownames(correlaciones), correlaciones$SalePrice))
colnames(entrenaCol.Correl.Price) <- c("variable", 'correlacion')
entrenaCol.Correl.Price <- arrange(entrenaCol.Correl.Price, desc(correlacion))
entrenaCol.Correl.Price
head(entrenaCol.Correl.Price) # Las mejores seis correlaiones con el SalePrice


# Lo vemos en View todas las correlaciones
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
  geom_point(color="red") + geom_smooth(method = "lm")

ggplot(entrena, aes(x=GarageArea, y=SalePrice)) +
  geom_point(color="red") + geom_smooth(method = "lm")


ggplot(entrena, aes(x=TotalBsmtSF, y=SalePrice)) +
  geom_point(color="orange") + geom_smooth(method = "lm")


ggplot(entrena, aes(x="1stFlrSF", y=SalePrice)) +
  geom_point(color="purple") + geom_smooth(method = "lm")


modelo <- lm(formula = SalePrice ~ OverallQual + 
               GrLivArea + GarageCars + GarageArea + 
               TotalBsmtSF , entrena)
modelo

summary(modelo)



