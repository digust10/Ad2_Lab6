tab5rows <- read.table("yoochoose-clicks.dat", header = FALSE, nrows = 5)
classes <- sapply(tab5rows, class)
clicks <- read.table("yoochoose-clicks.dat", header = FALSE, nrows = 2000000, colClasses = classes, sep = ",")
colnames(clicks) = c("session","timestamp","itenid","category")

clicks$timestamp = as.numeric(as.character(substring(clicks$timestamp, 12, 13)))

write.table(clicks, file="clicKsLim.csv")

tab5rows <- read.table("yoochoose-buys.dat", header = FALSE, nrows = 5)
classes <- sapply(tab5rows, class)
buys <- read.table("yoochoose-buys.dat", header = FALSE, nrows = 1150753, colClasses = classes, sep = ",")
colnames(buys) = c("session","timestamp","itenid","price","quantity")

buys$timestamp = as.numeric(as.character(substring(buys$timestamp, 12, 13)))
write.table(buys, file="buysLim.csv")


#merge das categorias
compra = intersect(clicksLim$session,buysLim$session)

clicksLim["buy"] <- 0
#clicksLim$buy <- match(clicksLim$session, compra)
clicksLim$buy <- clicksLim$session %in% compra


#terinando o modelo de compra em potencial

library("C50")

clicksLim$X <- NULL
#clicksLim$category <-NULL
fac_buy <- factor(clicksLim$buy)
buy_model <- C5.0(clicksLim[,-5], fac_buy)
summary(buy_model)

#lembrar de tirar o nrow
teste = doismilhoes
#teste <- read.table("yoochoose-test.dat", nrows = 2000000,header = FALSE, sep = ",")
teste <- read.table("doismilhoes.csv", nrows = 2000000,header = FALSE, sep = " ")
colnames(teste) = c("session","timestamp","itenid","category")
teste$timestamp = as.numeric(as.character(substring(teste$timestamp, 12, 13)))
teste$buyPrev <- FALSE
write.csv(teste, file="teste2.csv")

p <- predict( buy_model, teste, type="class" )
c = data.frame(teste$session,teste$timestamp,teste$itenid,teste$category,p)
#ver esse category
write.csv(file="predicaoDeBuys.csv", x=c)

#preparando o rankin
itemsRankin <- read.table("C:/Users/digust10/Desktop/AD2/Lab6/items-v.csv", quote="\"")
itemPrices <- read.table("C:/Users/digust10/Desktop/AD2/Lab6/itemPrices-v2-edited.csv", quote="\"")

colnames(itemsRankin) = c("session","category","vendability")
itemsRankin["price"] <- 0
itemsRankin["recomendador"] <- 0
count = 1
for(i in 1:nrow(itemsRankin)){
  print(i)
  if(itemsRankin[i,1] > itemPrices[count,1]){
    inc(count)
  }
  if(itemsRankin[i,1] == itemPrices[count,1]){
    itemsRankin[i,4] = itemPrices[count,2]
    inc(count)
  }else{
    itemsRankin[i,4] = 0
  }
}

inc <- function(x)
{
  eval.parent(substitute(x <- x + 1))
}

#Somando o ranking
valor = 0
for(i in 1:nrow(itemsRankin)){
  print(i)
  valor = itemsRankin[i,3] + (1046/itemsRankin[i,4])

  if(!is.infinite(valor)){
    itemsRankin[i,5] = valor
  }else{
    itemsRankin[i,5] = 0
  }
}

x = itemsRankin[order(itemsRankin$category,-(itemsRankin$recomendador) ),]
write.csv(x, file = "rankinFinal.csv")

#Fazendo a predicao
predicaoDeBuys <- read.csv("C:/Users/digust10/Desktop/AD2/Lab6/predicaoDeBuys.csv")
rankinFinal <- read.csv("C:/Users/digust10/Desktop/AD2/Lab6/rankinFinal.csv")
PegandoRankin <- read.csv("C:/Users/digust10/Desktop/AD2/Lab6/PegandoRankin.csv", header=FALSE)


predicaoDeBuys["itensRec"] <- 0
#nao pegar a tabela de predicao de buys de uma vez
for(i in 1:nrow(predicaoDeBuys)){
  if(predicaoDeBuys[i,5]==TRUE){

    for(k in 1:nrow(PegandoRankin)){
      if(predicaoDeBuys[i,4] = PegandoRankin[k,3]){
        predicaoDeBuys[i,6] = paste(PegandoRankin[k,2],PegandoRankin[(k+1),2],PegandoRankin[(k+2),3],PegandoRankin[(k+3),3],PegandoRankin[(k+4),3])
      }
    }
  }
}