####################
#Período máximo de análise, em anos:
periodo = 20

#Médias móveis
MaCurta = 15
MaLonga = 32

#Duração de hold: (Tempo de permanência da posição comprada ou vendida, em dias de pregão (úteis))
dias = 20
####################


library(readxl)
stock <- read_excel("C:/Users/Arthur/OneDrive/Área de Trabalho/FEA USP/ESTATISTICA/STOCKPRICE.xlsm", sheet = "PAINEL")
precos1 = stock$Close
precos = c()

i = nrow(stock)-252*periodo
if (nrow(stock) <= 252*periodo) i = 1
while (i <= nrow(stock)){
  precos = c(precos, precos1[i])
  i = i + 1
}

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

titulo1 = paste(stock$TICKER[1],"- Média móvel curta:", MaCurta)
titulo2 = paste(stock$TICKER[1],"- Média móvel longa:", MaLonga)
periodo_considerado = as.numeric(format(Sys.Date(), "%Y"))-as.numeric(substring(stock$Date[1],1,4))
#plot(mediaLonga, main = titulo2, ylab = "Cotação", xlab = paste("Período Considerado:",periodo,"anos"))
plot(mediaCurta, main = titulo1, ylab = "Cotação", xlab = paste("Período Considerado:",periodo_considerado,"anos"))


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

if (length(stock$Date[cruzamentos_pcima])!=length(ganhoseperdas)){
  ganhoseperdas = c(ganhoseperdas, 1:(length(cruzamentos_pcima)-length(ganhoseperdas))/1000000)
}
plot(stock$Date[cruzamentos_pcima], ganhoseperdas, 
     xlab = "Momento da Operação", ylab = "Retorno %", 
     main = paste(stock$TICKER[1],"- Distribuição das operações e retornos"))
abline(coef = c(0,0))


### SALDO DE LONG E DE SHORT ###
saldo_long = substring((mgwin_long + mgloss_long)*100,1,5)
saldo_short = substring((mgwin_short + mgloss_short)*100,1,5)


cat("Ticker:",stock$TICKER[1],"/ Média móvel curta:", MaCurta, "/ Média móvel longa:", MaLonga,"
Duração das operações:", dias,"dias úteis
Período considerado:",periodo_considerado,"anos - Desde",as.numeric(substring(stock$Date[1],1,4)),"\n
Total de operações (cruzamentos):", contagem_cruzamentos_total,"
Cruzamentos para cima:", contagem_cruzamentos_pcima,"
Cruzamentos para baixo:", contagem_cruzamentos_pbaixo,"\n
wins compra:", contagem_wins_compra, "- acertividade:", acertividade_compra,"%
wins venda:", contagem_wins_venda, "- acertividade:", acertividade_venda,"%
wins total:", contagem_wins_total, "- acertividade:", acertividade_total,"%\n
Rentabilidade acumulada:
Comprado: ", saldo_long,"% / Vendido: ", saldo_short,"%")