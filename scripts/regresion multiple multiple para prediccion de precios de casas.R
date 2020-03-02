# Prediccion de casas

library(readr)
library(dplyr)
library(ggplot2)
library(reshape) # Para renombrar columnas
# LIBRERIAS


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
ggplot(entrena, aes(x=SalePrice)) + geom_histogram()


# DEPURAR ******
# Cómo detectar cuáles variables a utilizar como independientes
# y que sólo son variables numéricas

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
# Se observa en View todas las correlaciones
View(correlaciones)

# Solo las correlaciones de todas las variables numéricas 
# con respecto al precio de venta
cor.soloPrecio <- select (correlaciones, SalePrice) %>%
  filter(SalePrice >= 0.5)
cor.soloPrecio

# Otra manera de saber las variables a partir de las correlaciones
# Primero hacer un dataframe de solo los nombres de variables y las correlaciones 
# con respecto al precio de venta y ponerles nombre de columna
entrenaCol.Correl.Price <- data.frame(cbind(rownames(correlaciones), correlaciones$SalePrice))
colnames(entrenaCol.Correl.Price) <- c("variable", 'correlacion')
entrenaCol.Correl.Price

# Crear un conjunto de datos ordenado de mayor a menor por el valor
# de la correlación del precio de ventas con las demás variables
# Determinar head(), las mejores seis correlaiones con el SalePrice
entrenaCol.Correl.Price <- arrange(entrenaCol.Correl.Price, desc(correlacion))
entrenaCol.Correl.Price
head(entrenaCol.Correl.Price) 

# Hay algunas correlacions cuyo valor es NA
cuales.son.NA.a.borrar <-filter(entrenaCol.Correl.Price, is.na(correlacion))
cuales.son.NA.a.borrar
# Quitamos esas columnas del conjunto de entrenamiento numéricas entrenaNumericas

#entrenaNumericas
ncol(entrenaNumericas)
entrenaNumericas[,!names(entrenaNumericas) %in% as.vector(cuales.son.NA.a.borrar$variable)]
entrenaNumericas <- entrenaNumericas[,!names(entrenaNumericas) %in% as.vector(cuales.son.NA.a.borrar$variable)]
ncol(entrenaNumericas)

# Las columnas mas correlaciones con SalePrice
# OverallQual, GrLivArea, GarageCars, GarageArea, TotalBsmtSF, 1stFlrSF

# plot
plot(x = entrenaNumericas$OverallQual, y=entrena$SalePrice)

# Con ggplot
ggplot(entrenaNumericas, aes(x=OverallQual, y=SalePrice)) +
  geom_point(color="darkred") + geom_smooth(method = "lm")

ggplot(entrenaNumericas, aes(x=GrLivArea, y=SalePrice)) +
  geom_point(color="darkgreen") + geom_smooth(method = "lm")

ggplot(entrena, aes(x=GarageCars, y=SalePrice)) +
  geom_point(color="red") + geom_smooth(method = "lm")

ggplot(entrenaNumericas, aes(x=GarageArea, y=SalePrice)) +
  geom_point(color="red") + geom_smooth(method = "lm")

ggplot(entrenaNumericas, aes(x=TotalBsmtSF, y=SalePrice)) +
  geom_point(color="orange") + geom_smooth(method = "lm")

ggplot(entrenaNumericas, aes(x="1stFlrSF", y=SalePrice)) +
  geom_point(color="purple") + geom_smooth(method = "lm")

# Determinar el modelo de regresión lineal en donde
# SalePrice es el precio de venta y la variable dependiente
# OverallQual, GrLivArea, GarageCars, GarageArea, TotalBsmtSF, 1stFlrSF
# son las variables independientes

# Cambiar el nombre de variable 1stFlrSF en conjunto de 
# entrenamiento de solo las variable numéricas
# que empieza con 1 a lstFlrSF que empezará zon l en entrena
# entrenaNumericas$lstFlrSF <- entrena$`1stFlrSF` # más práctico; opcion 1, crea nueva variable .. No es tan adecuado
entrenaNumericas = rename(entrenaNumericas, c(`1stFlrSF`="lstFlrSF"))  # opcion 2, renombra la variable

# Crear un modelo con ciertas variables numéricas
# las que tienen valor de correlación con precio de venta 
# superiro a 0.5
modelo1 <- lm(formula = SalePrice ~ OverallQual + 
               GrLivArea + GarageCars + GarageArea +
               TotalBsmtSF +  + FullBath + lstFlrSF +
                TotRmsAbvGrd + YearBuilt + YearRemodAdd, entrenaNumericas)
# Es engañar a R con un comentario en bloque
" Se identificaron las variables OverallQual, GrLivArea, 
GarageCars, GarageArea, TotalBsmtSF, 1stFlrSF que tienen
un valor de correlacion con respecto al precio de venta entre"

" Las variables que incluye el modelo de regresión lineal, 
representan el 77.37 y 77.21% de la variabilidad del 
precio de venta de las propiedades. "

modelo1

summary(modelo1)

# Interpretación Multiple R-squared:  0.7737,	Adjusted R-squared:  0.7721 

print("Al igual que ocurre en los modelos lineales simples, R2 
(coeficiente de determinación) es un cuantificador de la 
bondad de ajuste del modelo. 
Se define como el porcentaje de varianza de la variable predictiva (Y)
que se explica mediante el modelo respecto al total de variabilidad.
Por lo tanto, permite cuantificar como de bueno es el modelo
para predecir el valor de las observaciones.")

cat("En los modelos lineales múltiples, cuantos más predictores
se incluyan en el modelo mayor es el valor de R2, ya que, 
por poco que sea, cada predictor va a explicar una parte 
de la variabilidad observada en Y. 
Es por esto que R2 no puede utilizarse para comparar modelos
con distinto número de predictores.")

cat("R2ajustado introduce una penalización al valor de R2 
por cada predictor que se introduce en el modelo. 
El valor de la penalización depende del número de predictores
utilizados y del tamaño de la muestra, es decir, 
del número de grados de libertad. 
Cuanto mayor es el tamaño de la muestra, más predictores 
se pueden incorporar en el modelo. 
R2ajustado permite encontrar el mejor modelo, aquel que 
consigue explicar mejor la variabilidad de Y con el menor 
número de predictores.")

print("Aqui vamos ..pendiente")


# Crear un modelo con todas las variables numéricas en el conjnto de
# datos de entrenamiento 'entrena'
# Antes cambiar nombres a ciertas variables que empiezan con caracter 2 y 3
#entrenaNumericas$tndFlrSF <- entrena$`2ndFlrSF`
#entrenaNumericas$t3SsnPorch <- entrena$`3SsnPorch`

entrenaNumericas = rename(entrenaNumericas, c(`2ndFlrSF`="tndFlrSF"))  # renombra la variable
entrenaNumericas = rename(entrenaNumericas, c(`3SsnPorch`="t3SsnPorch"))  # renombra la variable


modelo2 <- lm(formula = SalePrice ~ ., data = entrenaNumericas)
modelo2

summary(modelo2)

# Con todas las variables cuantitativas
# Multiple R-squared:  0.809,	Adjusted R-squared:  0.8047 
# Representan el 80.9 y 80.47 de la variabilidad dleprecio de venta
# ¿Cuales variables tienen menor valor de estadísticamente significativas en la relación?
#MSSubClass    -1.620e+02  2.648e+01  -6.118 1.22e-09 ***
#  LotArea        3.935e-01  1.017e-01   3.868 0.000115 ***
#  OverallQual    1.787e+04  1.196e+03  14.936  < 2e-16 ***
#  OverallCond    4.438e+03  1.033e+03   4.296 1.86e-05 ***
#  YearBuilt      3.466e+02  6.117e+01   5.666 1.77e-08 ***
#  BsmtFinSF1     2.164e+01  4.682e+00   4.621 4.16e-06 ***
# lstFlrSF       5.074e+01  5.804e+00   8.741  < 2e-16 ***
#  tndFlrSF       5.065e+01  4.984e+00  10.161  < 2e-16 ***
# BedroomAbvGr  -1.051e+04  1.711e+03  -6.142 1.05e-09 ***
#  TotRmsAbvGrd   5.160e+03  1.249e+03   4.130 3.84e-05 ***
#  GarageCars     1.067e+04  2.883e+03   3.700 0.000224 ***
# ScreenPorch    5.781e+01  1.735e+01   3.331 0.000887 ***

modelo3 <- lm(formula = SalePrice ~ LotArea + 
                OverallQual + OverallCond + YearBuilt +
                BsmtFinSF1 + lstFlrSF + tndFlrSF +
                BedroomAbvGr + TotRmsAbvGrd + GarageCars +ScreenPorch, 
              entrenaNumericas )
modelo3

summary(modelo3)

# ¿Cual modelo de regresión lineal múltiple tomar y porqué?
summary(entrenaNumericas$SalePrice)

# Otra vez
cat("R2ajustado introduce una penalización al valor de R2 
por cada predictor que se introduce en el modelo. 
El valor de la penalización depende del número de predictores
utilizados y del tamaño de la muestra, es decir, 
del número de grados de libertad. 
Cuanto mayor es el tamaño de la muestra, más predictores 
se pueden incorporar en el modelo. 
R2ajustado permite encontrar el mejor modelo, aquel que 
consigue explicar mejor la variabilidad de Y con el menor 
número de predictores.")


# MArgen para mejora
# Utilizar el paquete MASS para eliminar variable autpmática en el modelo
# Se intenta eliminar variables del modelo2
library(MASS)

# Hacer una selección de las variables para dentifiar cuales son importantes y cuáles no
print("Cuando se dispone de muchas variables explicativas 
potenciales, las estrategias de regresión anteriores definen 
normalmente un subconjunto posible de modelos y el problema 
radica en seleccionar el mejor entre ellos")

# Para ello se puede utilizar distintos estadísticos, 
# como el criterio de información de Akaike AIC, 
# el criterio de información Bayesiana BIC, 
# el coeficiente de determinación R2, 
# el coeficiente de determinación corregido R2aj, 
# la varianza residual o el estadístico Cp de Mallows.
print("La step del paquete stats y stepAIC del paquete MASS, 
      que permite seleccionar el método mediante el argumento 
      direction = c(«both», «backward», «forward»). 
      Utilizan el AIC como criterio de selección de variables.")


step.modelo2 <- stepAIC(modelo2, direction = "backward")

summary(step.modelo2)

# Residual standard error: 35040 on 1439 degrees of freedom
# Multiple R-squared:  0.8081,	Adjusted R-squared:  0.8055
# Cual modeolo elegir ?
