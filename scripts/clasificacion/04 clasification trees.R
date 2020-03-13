# arboles de clasificacion 
install.packages(c("rpart", "rpart.plot", "caret"))

library(caret)
library(rpart)
library(rpart.plot)

banknote <- read.csv("data/tema3/banknote-authentication.csv", 
                     header = TRUE, stringsAsFactors = F, na.strings = TRUE)

banknote

set.seed(2018)

# Creamos los datos de entrenamiento
triningIds <- createDataPartition(banknote$class, p=0.7, list = FALSE)

# Generamos el modelo del árbol aleatorio con diferentes parámetros
# class~ . Es dependiente de todas las demas variables
# class~ . <=> "EQUIVALENTE" class ~ variance + skew + curtosis + entropy
# Se especifican las variable depndiente de las independientes
# class signifca clasificar
# method es una clasificación o un árbol que clasifique casos. Arbol de clasificación
# rpart.control El árbol tiene que considerar 0 casos y cp para par ajustar la complejidad
# Son valores por defecto. minsplit = 20, cp=0.01
mod <- rpart(class~ ., data = banknote[triningIds,],
             method = "class",
             control = rpart.control(minsplit = 20, cp=0.01) )

mod  # Esto es el árbol

# extra = 106 o 104 para ver binarioo texto
# 
prp(mod, type = 2, extra = 104,
    nn=TRUE, 
    fallen.leaves = TRUE, 
    faclen = 4, 
    varlen = 8 
    #shadow.col = "red"
    )


# Vamos a podar el árbol
mod$cptable

mod.pruned <- prune(mod, mod$cptable[6,"CP"])

prp(mod.pruned, type = 2, extra = 104,
    nn=TRUE, 
    fallen.leaves = TRUE, 
    faclen = 4, 
    varlen = 8 
    #shadow.col = "red"
    )
