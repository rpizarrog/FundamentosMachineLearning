# Regresión logística
install.packages("tidyverse")
install.packages("ISLR")

library(tidyverse)
library(ISLR)
datos <- Default

# Se recodifican los niveles No, Yes a 1 y 0
datos <- datos %>%
  select(default, balance) %>%
  mutate(default = recode(default,
                          "No"  = 0,
                          "Yes" = 1))

# Ajuste de un modelo lineal por mínimos cuadrados.
modelo <- lm(default ~ balance, data = datos)

# Representación gráfica del modelo.
ggplot(data = datos, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  geom_smooth(method = "lm", color = "gray20", se = FALSE) +
  theme_bw()  +
  labs(title = "Regresión lineal por mínimos cuadrados",
       y = "Probabilidad default") +
  theme(legend.position = "none")
head(datos)


cat ("Al tratarse de una recta, si por ejemplo, 
     se predice la probabilidad de default para alguien
     que tiene un balance de 10000, 
     el valor obtenido es mayor que 1.
     No es del todo eficiente. 1.2235")
predict(object = modelo, newdata = data.frame(balance = 10000))

cat("La regresión logística transforma el valor devuelto por la 
    regresión lineal (β0+β1X) empleando una función cuyo 
    resultado está siempre comprendido entre 0 y 1")
# Ajuste de un modelo logístico.
modelo <- glm(default ~ balance, data = datos, family = "binomial")

summary(modelo)

# Representación gráfica del modelo.
ggplot(data = datos, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  stat_function(fun = function(x){predict(modelo_logistico,
                                          newdata = data.frame(balance = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")

# Con geom_smooth se puede obtener el gráfico directamente.
ggplot(data = datos, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "gray20",
              se = FALSE) +
  theme_bw() +
  theme(legend.position = "none")


# Prediccion de nuevos valores
prediccion <- predict(object = modelo, newdata = data.frame(balance = 2500), se.fit = TRUE)
prediccion

predicciones.prob <- exp(prediccion$fit) / (1 + exp(prediccion$fit))
predicciones.prob

salida <- ifelse(predicciones.prob > 0.5, yes = 1, no = 0)
salida

