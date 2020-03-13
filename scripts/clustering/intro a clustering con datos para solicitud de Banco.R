# Caso de Aceptación de una solicitud en un Banco 
# Los datos relacionados con Solicitud de crédio hacia un Banco

# Las librerías
library(dplyr)
library(readr)
library(foreign) 

id <- 1:13
nombre <- c("Jose","Pedro","Maria","Anna", "Leo","Eva","Alex","Miren",
          "Beatriz","Cristian","Martha","Vero","Adris")

edad <- c(22, 45, 38, 41, 29, 40, 63, 17, 22, 36, 52, 51, 19)

sexo <- c("H","H","M","M", "H","M","H","M","M","H","M","M","M")
sexo <- as.numeric(sexo=="H")

ingresosMes <- c(7000, 85000, 26000, 117000, 0, 32000,
               130000, 6500, 8900, 8600, 29000, 80000, 13000)

tarjetasMes <- c(0, 40000, 12000, 50000, 0, 12000,
               80000, 0, 0, 0, 10000, 35000, 0)

hipotecasMes <- c(0, 32000, 0, 41000, 0, 0,
                35000, 0, 0, 0, 0, 3000, 0)

tarjetasMesU <- c(0, 0.85, 1, 0.8, 0, 1, 0.7, 0, 0, 0, 0.4, 0.5, 0)

solicitud <- c("No", "Si", "Si", "No", "No", "Si",
             "Si", "No", "No", "No", "No", "Si", "Si")

solicitud<-solicitud=="Si" # Transforma a TRUE y FALSE

datos <- data.frame(id, nombre, edad, sexo,
                    ingresosMes,tarjetasMes, 
                    hipotecasMes, tarjetasMesU, solicitud)

# Las librerías
library(foreign) 

# Ahora, se estandarizan las variables que se utilizarán 
# para la categorización, esto así para evitar que la distancia 
# calculada esté incidida por la escala de las variables.
datosEscalados <- scale(datos[ ,-c(1,2,9)])

row.names(datosEscalados) <- nombre

datosEscalados

# MatrimatrizDist = hclust(dist(datoslistos)^2, method = "average")
matrizDist$merge

# Interprtración de la matriz de distancias ...

cat("La forma de interpretar la salida anterior es que
    el primer paso el algoritmo agrupó a la 
    observación 3 con la 6 ([1,]   -3   -6") 
cat ("en el segundo paso agrupó la observación 8 
     con la 13 ([2,]   -8  -13)")
cat("en el tercer paso, agrupó la observación 9, 
    con las dos observaciones que se agruparon 
    en el paso 2 ([3,]   -9    2), 
    y así de forma sucesiva.")
cat("De esta manera, puede verse que números negativos
    indican asociación entre observaciones, 
    mientras que los positivos indican asociación de una
    observación, con algunas observaciones ya agrupadas
    previamente.")

plot(matrizDist,main="Dendograma",
     labels=row.names(datoslistos),hang=-1)

cat("1. ¿Que tienen en común Alex, Pedro y Ana?")
cat("R1. Son los que mas ganan")

cat("2. ¿Que parecido en los datos hay entre María, Eva, Martha y Vero?")
cat("")

filter(datos, nombre == 'Alex' | nombre == 'Pedro' | nombre == 'Anna')

