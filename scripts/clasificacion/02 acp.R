# Análisis de Componentes Princiales

usaarrest <- read.csv("data/tema3/USArrests.csv", 
               header = TRUE, stringsAsFactors = F, na.strings = TRUE)

usaarrest

rownames(usaarrest) <- usaarrest$X

usaarrest

# Eliminar x
usaarrest$X <- NULL

usaarrest

apply(usaarrest, 2, var)

# prcomp ejecuta un análisis de componentes principales
# center significa restarle la media y dividir la dessviaci´pn entonces normaliza
# scale significa 
acp <- prcomp(usaarrest, center = TRUE, scale = TRUE)
acp

par(mfrow=c(1,1))
plot(acp, type = "l")  # Recordar la técnica del codo para quedarse con los dos promeros componentes
summary(acp)   # Poder explicar un porcentaje de la variabilidad de los datos
# Para este summary por ejemplo completar hasta el 80% e la variabilidad de los adtos
# entonces basta con los dos primeros componentes

biplot(acp, scale= 0) # Que bonito ...

# Vamos a entender un poco lo que genera la matriz de rotación
# Primero se dice que son valores en otro espacio de dimensión?, otro espacio?
# Los primeros vectores son los que importan mas, los que explican mas de la varianza
# Esto es de mi cosecha. 11-05-2019
# Se dice que lo que genera la rotación es una matriz octogonal
mt <- acp$rotation ; mt

# De hecho lo que genera las rotaciones es una matriz octogonal con 
# valores de correlación
# ¿Que es una matriz octogonal ?
# Matriz Octogonal: matriz que multiplicada por su traspuesta resulta 
# la matriz identidad (A · AT = I)
# Entonces, si multiplicamos en R la matriz por su transpuesta debe dar
# la matriz identidad

round(mt %*% t(mt),0) # Redondeado

# Un dato de referencia de ACP
# La búsqueda de estos nuevos ejes se hace mediante el cálculo de los 
# llamados valores propios y vectores propios de la matriz de correlaciones 
# entre todas las variables del estudio. 
# Puede hacerse también a partir de otra matriz, la de varianzas-covarianzas, 
# pero ésta tiene el problema de que cuando las variables tienen unidades de 
# escala muy diferentes introduce un exceso de influencia por parte de las 
# variables con mayor varianza. 
# Por esto suele trabajarse con la matriz de correlaciones. 
# De esta forma se unifica el peso de las variables iniciales del estudio. 
# Suele hablarse de variables estandarizadas cuando se trabaja con la matriz
# de correlaciones. 
# Una variable es estandarizada cuando la muestra se transforma a media cero 
# y Desviación estándar uno. 
# Esto se hace restando a cada valor muestral la media muestral y 
# dividiendo por la Desviación estándar. 
# De esta forma todas las variables del estudio tienen la misma media y 
# la misma Desviación estándar y ninguna pesa más que otra. 
# De esta forma la vocalización del estudio se pone en cómo es la forma 
# de la nube de puntos, de cuáles son las relaciones entre las variables 
# que permiten reducir dimensiones perdiendo el mínimo de información.
# Fuente: https://estadisticaorquestainstrumento.wordpress.com/2012/12/29/tema-17-analisis-de-componentes-principales/
# hay que interpretar el juego de fuerzas de los signos.
# Aqui continuamos
print(acp)

pc1 <- apply(acp$rotation[,1] * usaarrest, 1, sum)
pc1  # Componente principa calculada

pc2 <- apply(acp$rotation[,2] * usaarrest, 1, sum)
pc2  # Componente principa calculada

biplot(acp)

plot (acp)

# Agregamos dos columnas
usaarrest$pc1 <- pc1
usaarrest$pc2 <- pc2
usaarrest


usaarrest2 <- usaarrest[,5:6] 

usaarrest2 

plot(usaarrest2)   # Que signifiará este plot



# Ejemplo I: PCA datos Económicos de Europa
Europa=read.csv("http://www.instantr.com/wp-content/uploads/2013/01/europe.csv", header=TRUE)
head(Europa, 3)

# Como podemos ver, este conjunto de datos tiene 1 variable categórica (Country)
# y las restantes numéricas. Por ello deberíamos quitar 
# la primera columna para poder llevar adelante el análisis:
Europa2=Europa[,2:8] # Quitar columa evitando la primera
head(Europa2, 3)


# A continuación utilizamos la función prcomp() para obtener las componentes principales:
pca.Europa2 <- prcomp(Europa2,scale=FALSE) # Scale = False -> utilizamos la matriz de COVARIANZA para obtener las componentes! Veamos que ocurre:

summary(pca.Europa2)

par(mfrow=c(1,1))
plot(pca.Europa2)

# Pero un solo componente que monopoliza todo ?
# Es que hay diferentes tipos de unidades medidas en cada variable
pca.Europa2 <- prcomp(Europa2,scale=T) # Scale = True -> utilizamos la matriz de CORRELACIÓN para obtener las componentes! 
#Veamos que ocurre:
summary(pca.Europa2)

plot(pca.Europa2)

# Podemos onocer mas cosas
pca.Europa2$sdev # Varianza de cada componente.

pca.Europa2$rotation # cargas de cada componente.

head(pca.Europa2$x, 3) # Matriz de datos (solo primeras 3 filas) con las componentes (en columnas las nuevas variables).

# Un ejemplo parecido
library(cluster) ## Vamos a utilizar este paquete para hacer 'clusters', un tema que estudiaremos en detalle mas adelante.
states=c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID", "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO", "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA", "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
states

raw <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/unempstates.csv")
raw[1:3,1:5] # Y las filas continuan hasta la dimensión 416

raw

# Transponemos la matriz
rawt=matrix(nrow=50,ncol=416)
rawt=t(raw)

rawt

# Caculamos los componentes

pcaunemp <- prcomp(rawt,scale=FALSE) # computamos las componentes  ( porque en este caso scaling = False ???)

pcaunemp

head(pcaunemp$sdev) # Aqui vemos las varianzas de las componentes

sum(head(pcaunemp$sdev))
# Haciendo diagramas del tabla
par(mfrow=c(1,1))

plot(pcaunemp, main="Varianza de las componentes")
mtext(side=1,"Unemployment: 50 states",line=1,font=2)


pcaunemp$rotation[1:10,1] ## Cargas de la primera componentes. Solo se visualizan las primeras 10 variables (recordemos que aquí tendremos una ecuación con 416 cargas para cada una de las variables en columna de la matriz de datos original)


# Calculamos la tasa media de paro para todos los estos para cada uno de los meses en el análisis.
ave=dim(416)
for (j in 1:416) {
  ave[j]=mean(rawt[,j])
} ### El vector 'ave' contiene 416 promedios.

ave

par(mfrow=c(1,2))
## Gráfica de los valores (negativos) de las cargas de la primera componente.
plot(-pcaunemp$rotation[,1], main='Cargas de la primera componente')
## Gráfica de los valores medios de paro para los estados
plot(ave,type="l",ylim=c(3,10),xlab="month",ylab="Evol. de la tasa de paro promedio")



# Otro ejemplo de análisis de componenes ACP

datos <- matrix(c(8,9,7,8,10,4,6,4,5,6,1,2,1,2,3), 
                nrow=5, ncol=3, 
                dimnames = list(1:5, c('LT', 'AM', 'Hm')))

datos

# La función prcomp devuelve un objeto de clase 'prcomp'. 
# Con el operador $ podemos acceder a lo siguiente:
# $rotation: son los autovectores
# $sdev: es la raíz cuadrada autovalores
# $x: nuevas coordenadas

# Sacando la covarianza
acp.cov <- prcomp(datos)

acp.cov


                
                              

