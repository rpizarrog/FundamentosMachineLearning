install.packages("e1071")

library(caret)
library(e1071)

banknote <- read.csv("data/tema3/banknote-authentication.csv", 
                     header = TRUE, stringsAsFactors = F, na.strings = TRUE)

banknote$class <- factor(banknote$class)

set.seed(2018)

t.Ids <- createDataPartition(banknote$class, p = 0.7, list = FALSE) # Para que devuelva array

t.Ids

# svm construye un modelo que clasifica
# class es la variable predictora
mod <- svm(class ~ ., data = banknote[t.Ids,])

# table crea una predicción
table(banknote[t.Ids, "class"], fitted(mod), dnn=c("Actual", "Predicción"))
pred <- predict(mod, banknote[-t.Ids, ]) # Son los datos de validación el 30%
table(banknote[-t.Ids, "class"], pred, dnn=c("Actual", "Predicción"))

# Plot, solo representar dos dimensiones, 
# es dificil representar cuatro o mas variables o dimensiones
plot(mod, data = banknote[t.Ids, ], skew ~ variance)
plot(mod, data = banknote[-t.Ids, ], skew ~ variance)
