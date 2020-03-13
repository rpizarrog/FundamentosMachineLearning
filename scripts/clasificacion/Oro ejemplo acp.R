# Otro ejejmplo de de ACP


# Referencia https://rpubs.com/Cristina_Gil/PCA. Muy bueno

install.packages("ISLR")
library(ISLR)

names(NCI60)

datos.nci <- NCI60$data # Solo una columna la data

head(datos.nci)
class(datos.nci)
str(datos.nci)

unique(NCI60$labs)

# Media de la expresión de cada gen (muestra de los 10 primeros). 
# (MARGIN = 2 para que se aplique la función a las columnas)
apply(X = datos.nci, MARGIN = 2, FUN = mean)[1:10]

# Por defecto, la función prcomp() centra las variables para que tengan media de 0.
# Con el argumento  scale = TRUE indicamos que queremos 
# escalar las variables para que tengan desviación estándar igual a 1.

pca.nci <- prcomp(datos.nci, scale = TRUE)

names(pca.nci)

# Muestra de los primeros 6 elementos del vector de loadings de los 5 primeros componentes
head(pca.nci$rotation)[, 1:5]

head(pca.nci$rotation)

dim(datos.nci) # Los datos
dim(pca.nci$rotation) # La rotada


summary(pca.nci)