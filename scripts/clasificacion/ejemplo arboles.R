# # En el campo del aprendizaje automático, hay distintas maneras
# de obtener árboles de decisión, la que usaremos en esta ocasión
# es conocida como CART: Classification And Regression Trees. 
# 
# Esta es una técnica de aprendizaje supervisado. 
# Tenemos una variable objetivo (dependiente) y nuestra meta
# es obtener una función que nos permita predecir, 
# a partir de variables predictoras (independientes),
# el valor de la variable objetivo para casos desconocidos.
# 
# Como el nombre indica, CART es una técnica con la que se
# pueden obtener árboles de clasificación y de regresión.
# Usamos clasificación cuando nuestra variable objetivo es 
# discreta, mientras que usamos regresión cuando es continua. 
# 
# Nosotros tendremos una variable discreta, así que haremos
# clasificación.
# 
# La implementación particular de CART que usaremos es conocida 
# como Recursive Partitioning and Regression Trees o RPART. 
# De allí el nombre del paquete que utilizaremos en nuestro ejemplo.
# De manera general, lo que hace este algoritmo es encontrar 
# la variable independiente que mejor separa nuestros datos en grupos, 
# que corresponden con las categorías de la variable objetivo. 
# Esta mejor separación es expresada con una regla. 
# A cada regla corresponde un nodo.
# Por ejemplo, supongamos que nuestra variable objetivo tiene dos niveles,
# deudor y no deudor. Encontramos que la variable que mejor separa nuestros 
# datos es ingreso mensual, y la regla resultante es que ingreso mensual > X pesos.
# Esto quiere decir que los datos para los que esta regla es verdadera, 
# tienen más probabilidad de pertenecer a un grupo, que al otro. 
# En este ejemplo, digamos que si la regla es verdadera, 
# un caso tiene más probabilidad de formar parte del grupo no deudor.
# 
# Una vez hecho esto, los datos son separados (particionados)
# en grupos a partir de la regla obtenida. Después, para cada 
# uno de los grupos resultantes, se repite el mismo proceso. 
# Se busca la variable que mejor separa los datos en grupos, 
# se obtiene una regla, y se separan los datos. 
# 
# Hacemos esto de manera recursiva hasta que nos es imposible
# obtener una mejor separación. Cuando esto ocurre,
# el algoritmo se detiene. 
# Cuando un grupo no puede ser partido mejor, 
# se le llama nodo terminal u hoja.
# 
# Una característica muy importante en este algoritmo es que una 
# vez que alguna variable ha sido elegida para separar los datos, 
# ya no es usada de nuevo en los grupos que ha creado. 
# Se buscan variables distintas que mejoren la separación de los datos.
# 
# Además, supongamos después de una partición que hemos creado dos grupos,
# A y B. Es posible que para el grupo A,
# la variable que mejor separa estos datos sea diferente a la que mejor 
# separa los datos en el grupo B. 
# Una vez que los grupos se han separado, al algoritmo “no ve” 
# lo que ocurre entre grupos, estos son independientes entre sí y
# las reglas que aplican para ellos no afectan en nada a los demás.
# 
# El resultado de todo el proceso anterior es una serie de bifurcaciones
# que tiene la apariencia de un árbol que va creciendo ramas, 
# de allí el nombre del procedimiento (aunque a mí en realidad 
# me parece más parecido a la raíz del árbol que a las ramas).
# 
# Las principales ventajas de este método son su interpretabilidad, 
# pues nos da un conjunto de reglas a partir de las cuales se pueden
# tomar decisiones. Este es un algoritmo que no es demandante en poder
# de cómputo comparado con procedimientos más sofisticados y, 
# a pesar de ello, que tiende a dar buenos resultados de predicción 
# para muchos tipos de datos.
# 
# Sus principales desventajas son que este en tipo de clasificación 
# “débil”, pues sus resultados pueden variar mucho dependiendo de la
# muestra de datos usados para entrenar un modelo. Además es fácil 
# sobre ajustar los modelos, esto es, hacerlos excelentes para 
# clasificar datos que conocemos, pero deficientes para datos conocidos.
# 

# referencia https://rpubs.com/jboscomendoza/arboles_decision_clasificacion

# **************************************** 
# Paquetes a utilizar

install.packages("tidyverse", dependencies = TRUE)
install.packages("rpart", dependencies = TRUE)
install.packages("rpart.plot", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)


library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

# ****************************************
# Importando nuestros datos
# Descargar datos
# Datos
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", "wine.data")

# Información
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.names", "wine.names")

dir() # Ya los tengo.... ¿ pero que son ?
readLines("wine.data", n = 10) # diez lineas


# Vamos a darle forma a los datos
vino <- read.table("wine.data", sep = ",", header = FALSE)

# Ahora que tiene wine.names
readLines("wine.names", n = 10)

# Parece ser un archivo de texto común y corriente, 
# pero con una extensión inusual. 
# Podemos crear una copia de este archivo con la 
# extensión a .txt con file.copy() para leerlo 
# fácilmente en bloc de notas o cualquier 
# aplicación similar. 
# Después, usamos  file.show() para darle una lectura

file.copy(from = "wine.names", to = "wine_names.txt")

file.show("wine_names.txt") # Explica lo que contienen los datos

# Aunque es probable que la primera columna de nuestros datos sea 
# la variable con el tipo de vino, usamos summary() para asegurarnos

summary(vino)

# Poner nombres de variables
nombres <- 
  readLines("wine_names.txt")[58:70] %>% 
  gsub("[[:cntrl:]].*\\)", "", .) %>% 
  trimws() %>% 
  tolower() %>% 
  gsub(" |/", "_", .) %>% 
  # Agregamos el nombre "tipo", para nuestra primera columna con los tipos de vino
  c("tipo", .)

names(vino) <- nombres 
vino

str(vino) # No e factor la variable tipo

# Hacerlo factor, la variable tipo de vino ,Hay otras maneras pero esta funciona
vino <- vino %>% 
  mutate_at("tipo", factor) 

# vino$tipo <- factor(vino$tipo) # Es lo mismo

vino
str(vino)  # Ahora si es factor

# ****************************************
# Creando un sets de entrenamiento y prueba

set.seed(1649)
vino_entrenamiento <- sample_frac(vino, .7) # El 70 porciento
vino_entrenamiento

vino_prueba <- setdiff(vino, vino_entrenamiento) # El 30 % restante
vino_prueba

# Un ejemplo de datos de entrenamiento y prueba simple
datos <- seq(1, 10, 1) # la secuencia de 10 números
datos <- as.data.frame(datos) # Que sea data.frame
muestra <- sample_frac(datos, 0.7)
prueba <- setdiff(datos, muestra)

muestra ; prueba
# Aqui termina este pequeño ejemplo

# ****************************************
# Retomamos 
# Entrenando nuestro modelo

arbol_1 <- rpart(formula = tipo ~ ., data = vino_entrenamiento)

# Que tenemos ?
arbol_1

# Lo visualizamos
rpart.plot(arbol_1)


# Usamos la función precict() con nuestro set de prueba 
# para generar un vector con los valores predichos 
# por el modelo que hemos entrenado, 
# especificamos el parámetro type = "class".
# argumento type = "class para clasificacion
prediccion_1 <- predict(arbol_1, newdata = vino_prueba, type = "class")
prediccion_1


prediccion_1 <- predict(arbol_1, newdata = vino_prueba, type = "class")

# Creamos matriz de confusion
confusionMatrix(prediccion_1, vino_prueba[["tipo"]])


# Un egundo entrenamiento
set.seed(7439)
vino_entrenamiento_2 <- sample_frac(vino, .7)

vino_prueba_2 <- setdiff(vino, vino_entrenamiento)

arbol_2 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_2)

prediccion_2 <- predict(arbol_2, newdata = vino_prueba_2, type = "class")

# Visualizamos
rpart.plot(arbol_2)

# Nuevamente matriz de confusion
confusionMatrix(prediccion_2, vino_prueba_2[["tipo"]])



# Un tercer entrenamiento
set.seed(8476)
vino_entrenamiento_3 <- sample_frac(vino, .7)

vino_prueba_3 <- setdiff(vino, vino_entrenamiento)

arbol_3 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_3)

prediccion_3 <- predict(arbol_3, newdata = vino_prueba_3, type = "class")

rpart.plot(arbol_3)

# Otra matriz de confsion
confusionMatrix(prediccion_3, vino_prueba_3[["tipo"]])

