# Caso ejemlo par ROC
# enfermos y sanos de diabetes
enfms <- rnorm( 100, 173, 2 )
sanos <- rnorm( 100, 167, 2 )

enfms
sanos

# Los datos
datos <- data.frame( c( enfms, sanos ),
                     as.factor( rep( c("T", "F" ), times = 1, each = 100 ) ) )
colnames( datos ) <- c( "val", "attr" )

datos

# Aqui hacemos la predicción y el performance o el rendimiento
pred <- prediction( datos$val, datos$attr )
perf <- performance(pred, measure = "tpr", x.measure = "fpr" )

# Visualizamos 
plot(perf, colorize = TRUE, type = "l" ) #out
abline(a = 0, b = 1 )


# Calcuar el área bajo la curva ? y eso ?
AUC <- performance( pred, measure = "auc")
AUCaltura <- AUC@y.values

# Calcular el punto de corte óptimo
cost.perf <- performance( pred, measure = "cost" )
opt.cut <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
#coordenadas del punto de corte óptimo
x<-perf@x.values[[1]][which.min( cost.perf@y.values[[1]] ) ]
y<-perf@y.values[[1]][which.min( cost.perf@y.values[[1]] ) ]
points(x,y, pch=20, col="red")

