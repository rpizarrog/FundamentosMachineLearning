# Regresión lineal para variables categóricas caso Servicio Tiempo hrs
meses.ultimo.servicio = c(2,6,8,3,2,7,9,8,4,6)
tipo.reparacion = c("eléctrico", "mecánico","eléctrico", "mecánico",
                    "eléctrico", "eléctrico", "mecánico", "mecánico",
                    "eléctrico", "eléctrico")
horas.necearias.reparacion <- c(2.9,3.0,4.8,1.8,2.9,4.9,4.2,4.8,4.4,4.5)

datos <- data.frame(meses.ultimo.servicio, tipo.reparacion, horas.necearias.reparacion)
names(datos) <- c('meses', 'tipo', 'horas')
str(datos)
summary(datos)

pairs(datos)

#datos$cylinders <- factor(datos$cylinders, 
#             levels = c(3,4,5,6,8),
#             labels = c('3c', '4c','5c','6c','8c'))

datos$tipo.num <- factor(datos$tipo, 
                                levels = c('eléctrico','mecánico'),
                               labels = c(1,0))
str(datos)

summary(datos)


modelo = lm(data=datos, formula=horas ~ meses + tipo.num)
modelo
summary(modelo)

par(mfrow=c(1,2))
plot(datos$tipo, datos$horas)

plot(datos$meses, datos$horas)
par(mfrow=c(1,1))