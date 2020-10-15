# Funciones para el CASO 3
# Crear funciones en R

#######################################################
# Esa función recibe un valor chr de estatura de jugador
# de fútbol en formato esperado "#´##" que significa pies y pulgadas estaturas
# La función genera la estatura en metros del tipo 1.60 o similar
festatura <- function (height) {
  estcm <- as.numeric(substring(height, 1,1)) * 30.48 + 
    as.numeric(substring(height, 3,4)) * 2.54
  estcm <- round(estcm / 100,2)
  estcm
}

# Función que convierte pesos de libras a kilogramos
# Falta asegurarse de que los tres primeros caracteres
# sean dígitos
flbskgs <- function(pounds) {
  pounds <- as.numeric(substring(pounds, 1,3))
  kilos <- round(pounds * 0.453592,2)
  kilos
}


# Recibe un atributo Value de entrada y regresa un valor e salida 
# pensando en el tipo de datos Value del conjunto de datos.FIFA
# Vienen datos como '€725K' hay que dejarlo numérico a 725
# Vienen daatos como '"€45M"' hay que dejarlo numérico a 45
# Si es M es MILLONES, Si es K es MILES. 1.6M 1600000; 1.6K 1600
# Entonces quitar el primero y último caracter
# Para propósito de indentificar medidas estadísticas
fcleanValue <- function(valor.in) {
  options(scipen=999) # Notacione NO científica
  valor.out <- substr(valor.in,2,nchar(valor.in)-1)
  
  valor.out <- as.numeric(valor.out)
  
  valor.out
}


  

# Probar funcion: festatura("6'2")

# Cargar unos datos
library(readr)

clubs.nation <- read.csv("../datos/clubs.nation.csv")
