fhola <- function () {
  print("Hola mundo desde la función")
}

fsumar <- function(a, b) {
  suma <- a + b
  suma
} 

foperaciones <- function(x,y,tipo=1) {
  if (tipo == 1) {
    res <- x + y
  }
  if (tipo == 2) {
    res <- x - y
  }
  if (tipo == 3) {
    res <- x * y
  }
  if (tipo == 4) {
    res <- x / y
  }
  if (tipo == 5) {
    res <- x ^ y
  }
  res
  
}



fordenar <- function(datos, columna) {
  # Esta función recibe un dataframe y el número de la columna
  #y ordena todo el df
  df.orden <- datos[order(datos[columna]),]
  df.orden
}
