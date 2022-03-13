library(readxl)
stock <- read_excel("C:/Users/Arthur/PASTA DO R/STOCKPRICE.xlsx")
View(stock)
precos = stock$Close
###################
min_macurta = 1
max_macurta = 100
min_malonga = 1
max_malonga = 100

passocurto = 1
passolongo = 1
  
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
         
         #titulo1 = paste("Média móvel curta:", MaCurta)
         #titulo2 = paste("Média móvel longa:", MaLonga)
         #plot(mediaCurta, main = titulo1)
         #plot(mediaLonga, main = titulo2)
         
         
         
         
         # Vamos fazer a contagem dos wins:
         win_compra = c()
         win_venda = c()
         
         for (momento in cruzamentos_pcima){
           if (momento+dias <= length(precos)){
             if (precos[momento + dias] > precos[momento]){
               win_compra = c(win_compra, momento)
             }
           }
         }
         
         for (momento in cruzamentos_pbaixo){
           if (momento+dias <= length(precos)){
             if (precos[momento + dias] < precos[momento]){
               win_venda = c(win_venda, momento)
             }
           }
         }
         
         contagem_wins_compra = length(win_compra)
         #contagem_wins_venda = length(win_venda)
         #contagem_wins_total = contagem_wins_compra + contagem_wins_venda
         
         
         
         acertividade_compra = contagem_wins_compra/contagem_cruzamentos_pcima
         #acertividade_venda = contagem_wins_venda/contagem_cruzamentos_pbaixo
         #acertividade_total = contagem_wins_total/contagem_cruzamentos_total
         
         
         matrix_compra[MaLonga,MaCurta] = as.numeric(substring(acertividade_compra*10000,1,4))
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