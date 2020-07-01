#TRABAJO DE CAMPO 
#MODELOS Y SIMULACIONES

#Grupo    Lambda    Mu
#2        1.3        2

#GENERACION DE VALORES
lambda <- 1.3
mu <- 2
ro <- lambda/mu
set.seed(lambda + mu)
TL <- rexp(100, lambda)
TM <- rexp(100, mu)
TC <- rexp(100)
time <- 1:100

#LOOP TIME!
for (i in time) {
print(i)
  
}
ajustar_vectores <- function(arreglo){
  result <- numeric(100)
  result <- c(arreglo[1])
  for(t in seq_along(arreglo){
      valor <- arreglo[t] + arreglo[t+1]
       result <- c(result,valor)
 }
  returnValue(result)
}

TL_mod <- ajustar_vectores(TL)
for (i in TL_mod) {
  print(i)
  
}

###################################

#Calcular Valores Teoricos
L   <- lambda / (mu - lambda)
LC1 <- L - ro 
LC2 <- (lambda)^2 / ((mu-lambda)*mu)
W   <- L / lambda
WC  <- lambda / (mu*(mu-lambda))
