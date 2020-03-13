# Regresión lineal para variables categóricas y numéricas
cat("Se dispone de un dataset que contiene información de 30 libros. 
    Se conoce del peso total de cada libro, el volumen que tiene 
    y el tipo de tapas (duras o blandas). 
    Se quiere generar un modelo lineal múltiple que permita
    predecir el peso de un libro en función de su volumen 
    y del tipo de tapas.")

# Las librerías
library(ggplot2)

# Los datos
datos <- data.frame(peso = c(800, 950, 1050, 350, 750, 600, 1075, 250, 700,
                             650, 975, 350, 950, 425, 725),
                    volumen = c(885, 1016, 1125, 239, 701, 641, 1228, 412, 953,
                                929, 1492, 419, 1010, 595, 1034),
                    tipo_tapas = c("duras", "duras", "duras", "duras", "duras", 
                                   "duras", "duras", "blandas", "blandas",
                                   "blandas", "blandas", "blandas", "blandas",
                                   "blandas", "blandas"))

# Explorando datos
str(datos)
summary(datos)
head(datos)

# Analizar la correlación entre cada par de variables cuantitativas
# y diferencias del valor promedio entre las categóricas
pairs(datos)


cor.test(datos$peso, datos$volumen, method = "pearson")

ggplot(data = datos, mapping=aes(x = tipo_tapas, y = peso, color=tipo_tapas)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")

cat("El análisis gráfico y de correlación muestran una
    relación lineal significativa entre la variable peso 
    y volumen. La variable tipo_tapas parece influir de forma
    significativa en el peso. Ambas variables pueden ser 
    buenos predictores en un modelo lineal múltiple para 
    la variable dependiente peso.")

# Generar el modleo de regresión lineal 
modelo <- lm(peso ~ volumen + tipo_tapas, data = datos)
modelo
summary(modelo)

# Interpetación de los coefieicnetes
cat("En el caso del predictor volume, si el resto de variables
    no varían, por cada unidad de volumen que aumenta el libro 
    el peso se incrementa en promedio 0.71795 unidades.")

cat("Cuando un predictor es cualitativo, uno de sus niveles 
    se considera de referencia (el que no aparece en la tabla de resultados) 
    y se le asigna el valor de 0. 
    El valor de la pendiente de cada nivel de un predictor cualitativo 
    se define como el promedio de unidades que dicho nivel está por 
    encima o debajo del nivel de referencia. 
    Para el predictor tipo_tapas, el nivel de referencia es tapas blandas 
    por lo que si el libro tiene este tipo de tapas se le da a la 
    variable el valor 0 y si es de tapas duras el valor 1. 
    Acorde al modelo generado, los libros de tapa dura son en promedio 184.04727 
    unidades de peso superiores a los de tapa blanda.")

cat("El modelo es capaz de explicar el 92.75% de la variabilidad 
    observada en el peso de los libros (R-squared: 0.9275). 
    El valor de R2-ajustado es muy alto y cercano al R2 (Adjusted R-squared: 0.9154)
    lo que indica que el modelo contiene predictores útiles. 
    El test F muestra un p-value de 1.455e-07 por lo que el modelo en conjunto 
    es significativo. Esto se corrobora con el p-value de cada predictor, 
    en ambos casos significativo.")

# https://rpubs.com/Joaquin_AR/226291
