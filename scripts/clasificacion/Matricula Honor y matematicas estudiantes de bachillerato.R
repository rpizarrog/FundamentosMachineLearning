# Matrícula de honor y matem´wticas de alumnos de bachillerato

# Las librerías
library(ggplot2)

# Los datos
matricula <- as.factor(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1,
                         0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1,
                         0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
                         0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                         1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0,
                         1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1,
                         1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1,
                         0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                         0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0,
                         0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0,
                         0, 0, 0, 0, 1, 0, 0, 0, 1, 1))
matematicas <- c(41, 53, 54, 47, 57, 51, 42, 45, 54, 52, 51, 51, 71, 57, 50, 43,
                 51, 60, 62, 57, 35, 75, 45, 57, 45, 46, 66, 57, 49, 49, 57, 64,
                 63, 57, 50, 58, 75, 68, 44, 40, 41, 62, 57, 43, 48, 63, 39, 70,
                 63, 59, 61, 38, 61, 49, 73, 44, 42, 39, 55, 52, 45, 61, 39, 41,
                 50, 40, 60, 47, 59, 49, 46, 58, 71, 58, 46, 43, 54, 56, 46, 54,
                 57, 54, 71, 48, 40, 64, 51, 39, 40, 61, 66, 49, 65, 52, 46, 61,
                 72, 71, 40, 69, 64, 56, 49, 54, 53, 66, 67, 40, 46, 69, 40, 41,
                 57, 58, 57, 37, 55, 62, 64, 40, 50, 46, 53, 52, 45, 56, 45, 54,
                 56, 41, 54, 72, 56, 47, 49, 60, 54, 55, 33, 49, 43, 50, 52, 48,
                 58, 43, 41, 43, 46, 44, 43, 61, 40, 49, 56, 61, 50, 51, 42, 67,
                 53, 50, 51, 72, 48, 40, 53, 39, 63, 51, 45, 39, 42, 62, 44, 65,
                 63, 54, 45, 60, 49, 48, 57, 55, 66, 64, 55, 42, 56, 53, 41, 42,
                 53, 42, 60, 52, 38, 57, 58, 65)
datos <- data.frame(matricula, matematicas)
head(datos, 6)
tail(datos,6)

# Tabla de casos de Honor y No Honor
table(datos$matricula)

ggplot(data = datos, aes(x = matricula, y = matematicas, color = matricula)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")

# Generar el modelo de regresión logística
modelo <- glm(matricula ~ matematicas, data = datos, family = "binomial")
summary(modelo)

# MEDIANTE BASE GRAPHICS SIN INTERVALOS DE CONFIANZA

# Codificación 0,1 de la variable respuesta
datos$matricula <- as.character(datos$matricula)
datos$matricula <- as.numeric(datos$matricula)

plot(matricula ~ matematicas, datos, col = "darkblue",
     main = "Modelo regresión logística",
     ylab = "P(matrícula=1|matemáticas)",
     xlab = "matemáticas", pch = "I")

# type = "response" devuelve las predicciones en forma de probabilidad en lugar de en log_ODDs
curve(predict(modelo, data.frame(matematicas = x), type = "response"),
      col = "firebrick", lwd = 2.5, add = TRUE)

# MEDIANTE GGPLOT2 INCLUYENDO INTERVALOS DE CONFIANZA

datos$matricula <- as.character(datos$matricula)
datos$matricula <- as.numeric(datos$matricula)

# Se crea un vector con nuevos valores interpolados en el rango de observaciones.
nuevos_puntos <- seq(from = min(datos$matematicas), to = max(datos$matematicas),
                     by = 0.5)

nuevos_puntos

# Predicciones de los nuevos puntos según el modelo. 
# Si se indica se.fit = TRUE se devuelve el error estándar de cada predicción
# junto con el valor de la predicción (fit).
predicciones <- predict(modelo, data.frame(matematicas = nuevos_puntos),
                        se.fit = TRUE)
predicciones

# Mediante la función logit se transforman los log_ODDs a probabilidades.
predicciones_logit <- exp(predicciones$fit) / (1 + exp(predicciones$fit))
predicciones_logit
# Se calcula el límite inferior y superior del IC del 95% sustrayendo e
# incrementando el logODDs de cada predicción 1.95*SE. Una vez calculados los
# logODDs del intervalo se transforman en probabilidades con la función logit.
limite_inferior       <- predicciones$fit - 1.96 * predicciones$se.fit
limite_inferior_logit <- exp(limite_inferior) / (1 + exp(limite_inferior))
limite_superior       <- predicciones$fit + 1.96 * predicciones$se.fit
limite_superior_logit <- exp(limite_superior) / (1 + exp(limite_superior))

# Se crea un dataframe con los nuevos puntos y sus predicciones
datos_curva <- data.frame(matematicas = nuevos_puntos,
                          probabilidad_matricula = predicciones_logit,
                          limite_inferior_logit = limite_inferior_logit, 
                          limite_superior_logit = limite_superior_logit)

ggplot(datos, aes(x = matematicas, y = matricula)) +
  geom_point(aes(color = as.factor(matricula)), shape = "I", size = 3) + 
  geom_line(data = datos_curva, aes(y = probabilidad_matricula),
            color = "firebrick") + 
  geom_line(data = datos_curva, aes(y = limite_inferior_logit),
            linetype = "dashed") + 
  geom_line(data = datos_curva, aes(y = limite_superior_logit),
            linetype = "dashed") + 
  theme_bw() +
  labs(title = "Modelo regresión logística matrícula ~ nota matemáticas",
       y = "P(matrícula = 1 | matemáticas)", y = "matemáticas") + 
  theme(legend.position = "null") +
  theme(plot.title = element_text(hjust = 0.5))

# Diferencia de residuos
# En R, un objeto glm almacena la "deviance" del modelo, así como la "deviance"
# del modelo nulo. 
dif_residuos <- modelo$null.deviance - modelo$deviance

# Grados libertad
df <- modelo$df.null - modelo$df.residual

# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))

# El mismo cálculo se puede obtener directamente con:
anova(modelo, test = "Chisq")

cat("Para este estudio se va a emplear un threshold de 0.5. Si la probabilidad de que la variable adquiera el valor 1 (matrícula) 
    es superior a 0.5, se asigna a este nivel, 
    si es menor se asigna al 0 (no matrícula).")

library(vcd)
predicciones <- ifelse(test = modelo$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(modelo$model$matricula, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion
