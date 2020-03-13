# cURVAS roc
#install.packages("ROCR")

library(ROCR)

data1 <- read.csv("data/tema3/roc-example-1.csv", 
                      header = TRUE, stringsAsFactors = F, na.strings = TRUE)

data2 <- read.csv("data/tema3/roc-example-2.csv", 
                  header = TRUE, stringsAsFactors = F, na.strings = TRUE)

# 1 Exito con la probabilidad
# 0 Fracaso con la probabilidad

pred1 <- prediction(data1$prob, data1$class)

pred1   # que sale

# Ahora el performance Que significa ?
# Formal class predictor para medir la eficiencia
# En que casos se obtiene un caso de verdadero positivo y 
# en que casos se obtiene un caso de falso positivo
perf1 <- performance(pred1, "tpr", "fpr") # Los nombre de variables


# Visualizando 
plot(perf1)
lines(par()$usr[1:2], par()$usr[3:4])


# A partir de que valores estar seguro de que da valores de VERDADEDOR POSITIVOS?
# Determinar el corte en los valores del dataframe.
# Se acceden con un @ porque son objetos estructura interna de datos de R

prob.cut1 <- data.frame(cut = perf1@alpha.values[[1]],
                        fpr = perf1@x.values[[1]],
                        tpr = perf1@y.values[[1]])

head(prob.cut1)

tail(prob.cut1)


prob.cut1[prob.cut1$tpr > 0.80, ] # Probabilidad mayor a 80%


# tomando el segundo caso
pred2 <- prediction(data2$prob, data2$class, 
                    label.ordering = c("non-buyer", "buyer")
                    )

pred2   # Que sale
perf2 <- performance(pred2, "tpr", "fpr") # Los nombre de variables

# Visualizando 
plot(perf2)
lines(par()$usr[1:2], par()$usr[3:4])  # Que sgnifica



# Algo de literatura de ROC
# Una curva ROC (Receiver Operating Characteristic) es una 
# representación gráfica que ilustra la relación entre la 
# sensibilidad y la especificidad de un sistema clasificador 
# para diferentes puntos de corte

