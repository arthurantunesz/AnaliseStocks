####################
#Período de análise, em anos:
periodo = 0

#Médias móveis
MaCurta = 40
MaLonga = 90

#Duração de hold: (Tempo de permanência da posição comprada ou vendida, em dias de pregão (úteis))
dias = 10
#(periodos)
####################


library(readxl)
library(readr)
library(readr)
library(readr)
EURUSD1 <- read_delim("C:/Users/Arthur/PASTA DO R/EURUSD1.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

precos = EURUSD1$Close



###################
min_macurta = 1
max_macurta = 100
min_malonga = 1
max_malonga = 100

passocurto = 2
passolongo = 5

dias = 10
###################
matrix_compra = matrix(0, ncol = max_macurta-(min_macurta-1), nrow = max_malonga-(min_malonga-1))
matrix_contagem_compra = matrix(0, ncol = max_macurta-(min_macurta-1), nrow = max_malonga-(min_malonga-1))


colnames(matrix_compra) = c(min_macurta:max_macurta)
colnames(matrix_contagem_compra) = c(min_macurta:max_macurta)

rownames(matrix_compra) = c(min_malonga:max_malonga)
rownames(matrix_contagem_compra) = c(min_malonga:max_malonga)


MaCurta = min_macurta
while (MaCurta <= max_macurta){
  
  MaLonga = min_malonga
  if (MaLonga <= MaCurta) MaLonga = MaCurta + 1
  
  while (MaLonga <= max_malonga) {



if (periodo != 0){
i = nrow(EURUSD1)-252*periodo
if (nrow(EURUSD1) <= 252*periodo) i = 1
while (i <= nrow(EURUSD1)){
  precos = c(precos, precos1[i])
  i = i + 1
}}

index = MaCurta
mediaCurta = c()
while (index <= length(precos)){
  
  x = index - (MaCurta - 1)
  soma = 0
  while (x <= index){
    soma = soma + precos[x]
    x = x + 1
  }
  
  mediaCurta[index] = soma/MaCurta
  index = index + 1
}

index = MaLonga
mediaLonga = c()
while (index <= length(precos)){
  
  x = index - (MaLonga - 1)
  soma = 0
  while (x <= index){
    soma = soma + precos[x]
    x = x + 1
  }
  
  mediaLonga[index] = soma/MaLonga
  index = index + 1
}

# Aqui, o "a" é o indice dos vetores mediaLonga e mediaCurta
# O vetor cruzamentos p cima e p baixo são os pontos "a" que ocorre cruzamento
a = 1
cruzamentos_pcima = c()
cruzamentos_pbaixo = c()
while (a <= length(precos)-1){
  if (!is.na(mediaCurta[a]) & !is.na(mediaLonga[a])){
    
    if (mediaCurta[a+1] > mediaLonga[a+1] & mediaCurta[a] <= mediaLonga[a]){
      cruzamentos_pcima = c(cruzamentos_pcima, a+1)
    }
    else if (mediaCurta[a+1] < mediaLonga[a+1] & mediaCurta[a] >= mediaLonga[a]){
      cruzamentos_pbaixo = c(cruzamentos_pbaixo, a+1)
    }
  }
  a = a + 1
}
contagem_cruzamentos_pcima = length(cruzamentos_pcima)
contagem_cruzamentos_pbaixo = length(cruzamentos_pbaixo)
contagem_cruzamentos_total = contagem_cruzamentos_pcima + contagem_cruzamentos_pbaixo

titulo1 = "Média móvel curta:"
titulo2 = "Média móvel longa:"
#plot(mediaLonga, main = titulo2, ylab = "Cotação", xlab = paste("Período Considerado:",periodo,"anos"))
#plot(mediaCurta, main = titulo1, ylab = "Cotação", xlab = paste("Período Considerado:",periodo,"anos"))


#################################################################################

# Vamos fazer a contagem dos wins:
win_compra = c()
win_venda = c()
#mg = magnitude dos win ou dos loss em comprado (long) ou vendido (short)
#a magnitude de win/loss significa a SOMA de ganho/perda, em porcentagem, de todas as operações long e short

mgwin_long = 0
mgloss_long = 0
saldo_long = 0

mgwin_short = 0
mgloss_short = 0
saldo_short = 0

ganhoseperdas = c()

#################### LONG ###############
for (momento in cruzamentos_pcima){
  if (momento+dias <= length(precos)){
    if (precos[momento + dias] > precos[momento]){ #win
      win_compra = c(win_compra, momento)
      
      mgwin_long = mgwin_long + (precos[momento + dias] - precos[momento])/precos[momento]
      ganhoseperdas = c(ganhoseperdas, 100*(precos[momento + dias] - precos[momento])/precos[momento])
      
    }
    else if (precos[momento + dias] <= precos[momento]){ #loss
      
      mgloss_long = mgloss_long + (precos[momento + dias] - precos[momento])/precos[momento]
      ganhoseperdas = c(ganhoseperdas, 100*(precos[momento + dias] - precos[momento])/precos[momento])
      
    }
  }
}

#################### SHORT ###############
for (momento in cruzamentos_pbaixo){
  if (momento+dias <= length(precos)){
    if (precos[momento + dias] < precos[momento]){ #win
      win_venda = c(win_venda, momento)
      
      mgwin_short = mgwin_short - (precos[momento + dias] - precos[momento])/precos[momento]
      
    }
    else if (precos[momento + dias] >= precos[momento]){ #loss
      
      mgloss_short = mgloss_short - (precos[momento + dias] - precos[momento])/precos[momento]
      
    }
  }
}

###############################################################################


contagem_wins_compra = length(win_compra)
contagem_wins_venda = length(win_venda)
contagem_wins_total = contagem_wins_compra + contagem_wins_venda

acertividade_compra = substring((contagem_wins_compra/contagem_cruzamentos_pcima)*100,1,5)
acertividade_venda = substring((contagem_wins_venda/contagem_cruzamentos_pbaixo)*100,1,5)
acertividade_total = substring((contagem_wins_total/contagem_cruzamentos_total)*100,1,5)

if (length(EURUSD1$DAY[cruzamentos_pcima])!=length(ganhoseperdas)){
  ganhoseperdas = c(ganhoseperdas, 1:(length(cruzamentos_pcima)-length(ganhoseperdas))/1000000)
}
#plot(EURUSD1$DAY[cruzamentos_pcima], ganhoseperdas, 
#     xlab = "Momento da Operação", ylab = "Retorno %", 
#     main = "Distribuição das operações e retornos")


### SALDO DE LONG E DE SHORT ###
saldo_long = substring((mgwin_long + mgloss_long)*100,1,5)
saldo_short = substring((mgwin_short + mgloss_short)*100,1,5)



matrix_compra[MaLonga,MaCurta] = as.numeric(substring(acertividade_compra,1,4))
matrix_contagem_compra[MaLonga,MaCurta] = as.numeric(contagem_cruzamentos_pcima)


MaLonga = MaLonga + passolongo

  }          
  MaCurta = MaCurta + passocurto
}

#View(matrix_contagem_compra)
#View(matrix_compra)

write.csv(matrix_compra, "C:/Users/Arthur/PASTA DO R//MATRIZ PROBABILIDADE.csv")
write.csv(matrix_contagem_compra, "C:/Users/Arthur/PASTA DO R//MATRIZ OCORRENCIAS.csv")
cat("Operação concluída! CSVs baixados.")

k = matrix_compra
kk = matrix_contagem_compra
library(plot3D)
hist3D (x = 1:nrow(k), y = 1:ncol(k), z = as.matrix(k),
        bty = "g", phi = 20,  theta = -600, colvar = as.matrix(k),
        xlab = "MA longa", ylab = "MA Curta", zlab = "Ocorrências", main = "Distribuição",
        col = "#0072B2", border = "black", shade = 0.8,
        ticktype = "detailed", space = 0.15, d = 2, cex.axis = 1e-9)

library("plot3Drgl")
plotrgl()