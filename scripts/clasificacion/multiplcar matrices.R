# Multiplicar matrices

datos <- c(2,3,2,1)
r <- 2
c <- 2
mat <- matrix(datos, r, c, byrow = TRUE)
mat

vect <- c(6,4)

vect

result <- mat %*% vect
result


# Necesito un numero que multiplique a vect y de el resultado

vect

# Hcemos un ciclo para ver cual es el maximo valor en el vector
for (x in 1:max(vect)) {
  #print(x)
  if(identical(as.matrix(vect  * x, nrow=2, ncol=1, byrow=TRUE), result)) {
#    print(paste(x, " SI es el valor de λ es el eigenvalor"))
    eigenValor = x
  }
#  else
#    print(paste(x, " NO es el valor de λ es el eigenvalor"))
}

print(paste("Valor de eigen ", eigenValor))

