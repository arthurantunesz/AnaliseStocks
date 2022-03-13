#Importar Dataset anexado com esse arquivo antes do source. (PETR3.csv)
PETR3 = c()
PETR3 = c(PETR3, read.csv("PETR3.csv", sep = ","))

attach(PETR3)
vetor <- c(PETR3$Último)
PETR3v <- c()
cont <- length(vetor)

while (cont > 0){
  PETR3v <- append(PETR3v, vetor[cont])
  cont <- cont - 1
}

PETR3d <- c()
i <- 2
dia1 <- 0
dia2 <- 0

while(i <= length(PETR3v)){
  dia2 <- PETR3v[i]
  dia1 <- PETR3v[i - 1]
  PETR3d <- append(PETR3d, dia2 - dia1)
}
cat('PETR3d: ', PETR3d)


#Essa parte já é além do pedido.

queda <- 0
alta <- 0
estavel <- 0
ind <- 1

while (ind <= length(PETR3d)){
  if (PETR3d < 0){
    queda <- queda + 1
  }else if (PETR3d >0){
    alta <- alta + 1
  }else{
    estavel <- estavel + 1
  }
}

cat('Quedas: ', queda, '| Altas: ', alta, '| Estável: ', estavel)


