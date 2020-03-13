library(readr)
library(dplyr)

# Datos de rendimiento de estudiantes
# Matriz de confusión
datos <- read.csv("datos/college-perf.csv", 
                  header = TRUE, 
                  stringsAsFactors = F, 
                  na.strings = TRUE)
datos

names(datos)
# SAT. Prueba de Razonamiento con valores de puntuación en un rango de 600 a 2400
# GPA. Una medida cuantitativa para evaluar el rendimiento académico de un estudiante
# Grado o Calificación por Letra
#   A	Excelente	4
#   B	Bueno	3
#   C	Adecuado	2
#   D	Apenas pasando	1
#   F	Reprobado	0
# Projects  
# Community
# Income
# Perf. Nota Real de un alumno
# Pred. Nota predecida

colnames(datos)

# Ordenar la variable en tipo de factores, 
# darle un sigificado el factor 
# de las calificaciones que exista una jerarquía
# Ahora es un factor con un orden
datos$Perf <- ordered(datos$Perf, 
                   levels=c("Low", "Medium", "High")
                   )

datos$Pred <- ordered(datos$Pred, 
                   levels=c("Low", "Medium", "High")
                   )

# Tabla de doble entrada o matriz de confusión
tabla <- table(datos$Perf, datos$Pred,  
               dnn = c("Actual", "Predecido"))
tabla

prop.table(tabla)   # Las probabilidades

round(prop.table(tabla, 1) * 100, 2) # Redondear, Lo hace por filas

round(prop.table(tabla, 2) * 100, 2) # Redondear, Lo hace por columna

# Haciendo diagramas del tabla
par(mfrow=c(1,1))
barplot(tabla, legend = TRUE, xlab="Nota predecida por el modelo")

# Mosaic plot simplente
mosaicplot(tabla, main="Eficiencia del modelo")

# Summary
summary(tabla)


