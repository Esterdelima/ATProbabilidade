#Questão 1 

metragem <- c( 48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41,
               55, 49, 61, 49, 49, 52, 55, 60, 52, 54, 57, 47, 66, 60, 53, 
               59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40)

dataF2 <- data.frame(metragem)

#LETRA A
y<-1:45
y=y[-7]
metragem <- append(y,93)
Frequencia_absoluta<-table(metragem)
Frequencia_relativa<-round(Frequencia_absoluta/sum(Frequencia_absoluta)*
                                   100,digits=2)
tabelafinal<-cbind(metragem,Frequencia_absoluta,Frequencia_relativa)

#ADICIONANDO OS VALORES NO ARQUIVO TXT
total<-c("Total",sum(Frequencia_absoluta),sum(Frequencia_relativa))
tabelafinal<-rbind(tabelafinal,total)
write.table(tabelafinal,"dados-frequencia.txt",dec=".",sep =
                    ",",quote=FALSE)

#QUESTÃO 1


library(dplyr)
library(e1071)  
library(tidyverse)
library(tidyquant)
library(moments)
library(kurtosis)
library(timetk)

metragem <- c( 48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41,
               55, 49, 61, 49, 49, 52, 55, 60, 52, 54, 57, 47, 66, 60, 53, 
               59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40)


#Letra B

#Media
MEDIA = round(mean(metragem), 2)
#Desvio Padrao
DESVIO_PADRAO = sd(metragem)
#Mediana
MEDIANA = median(metragem)
#Q3
Q3 = quantile(metragem, 0.25)
#p8
P8 = quantile(metragem, 0.08)
#p50
P50 = quantile(metragem, 0.50)
#p80
P80 = quantile(metragem, 0.80)
#AP2
AP2 = skewness(metragem, na.rm = TRUE)
#K
K = kurtosis(metragem,  na.rm = TRUE)

ID = c("Média","Desvio Padrão","Madiana","Q3","P8","P50","P80", "AP2", "K" )
output = c(MEDIA, DESVIO_PADRAO, MEDIANA, Q3, P8, P50, P80, AP2, K)
table <- data.frame(id = ID, valores = output,  stringsAsFactors = FALSE)

#Letra C

boxplot(metragem,
        range = 1,
        main="",
        xlab="",
        ylab="Metragem",col="purple",vertical = TRUE)


#QUESTÃO 2

#CRIAÇÃO DA TABELA EM R 
filename1 <- "dadosgeral.csv"
tabelaCorona <- read.csv(filename1, header = TRUE, sep =',')

#EXECUTAR A LEITURA
str(tabelaCorona)

#MONTAGEM DA TABELA
dataF1 <- data.frame(tabelaCorona)


#LETRA A


#LETRA B
#VARIAVEL QUE VAI RECEBER A FREQUENCIA DE CASOS
situacao <- table(tabelaCorona$Mes)
str(situacao)

#RETORNANDO A FREQUENCIA ABSOLUTA
print("Frequência absoluta:")
print(situacao)

ord <- order(situacao)
situacao <- situacao[ord]
n <- names(situacao)
barplot(situacao,main="Situação atual dos casos mês a mês",las =1,col =
                rainbow(12),
        cex.names = 0.72,
        xlab = "Meses",
        ylab = "Número de casos",
        cex.axis = 0.8,
        cex.lab = 1,
        ylim = c(0, 800))

#LETRA C
#VARIAVEL QUE VAI RECEBER A FREQUENCIA DE CASOS
situacaoSexo <- table(tabelaCorona$Sexo)
str(situacaoSexo)

#RETORNANDO A FREQUENCIA ABSOLUTA
print("Frequência absoluta:")
print(situacaoSexo)

barplot(situacaoSexo,main="Situação atual dos casos por sexo",las =1,col =
                rainbow(2),
        cex.names = 0.72,
        xlab = "Sexo",
        ylab = "Número de casos",
        cex.axis = 0.8,
        cex.lab = 1,
        ylim = c(0, 2000))

#LETRA D
situacaoIdade <- table(tabelaCorona$Idade)
str(situacaoIdade)

#RETORNANDO A FREQUENCIA ABSOLUTA
print("Frequência absoluta:")
print(situacaoIdade)

barplot(situacaoIdade,main="Situação de obitos por Idade",las =1,col =
                rainbow(12),
        cex.names = 0.72,
        xlab = "Idade",
        ylab = "Número de casos",
        cex.axis = 0.8,
        cex.lab = 1,
        ylim = c(0, 120))






#QUESTÃO 3
filename <- "dados.csv"
tabelaProbabilidade <- read.csv(filename, header = TRUE, sep =',')

#Executei a leitura
str(tabelaProbabilidade)

#Montagem da tabela (agrupagem da tabela)
dataF <- data.frame(tabelaProbabilidade)

#associação da tabela com os dados
comparacao <- table(tabelaProbabilidade$Idade)
str(comparacao)


print("Frequencia absoluta:")
print(comparacao)

#histograma de frequencias de obitos
hist(dataF$Idade,breaks=30,main = "Frequencia de obitos relacionado com a idade",
     xlab="Idades",ylab="",xlim=c(20,100),ylim=c(0,5),col="red")

#histograma de idadexcasosxsexo
idadeMasc <- dataF$Idade[which(dataF$Sexo=='Masculino')]

idadeFem <- dataF$Idade[which(dataF$Sexo=='Feminino')]

hist(dataF$Idade,breaks=30,main = "Frequencia de Obitos",
     xlab="Idades",ylab="",xlim=c(20,90),ylim=c(0,5),col="red")
legend("topright",inset=.02,title="Sexo",c("feminino")
       ,fill = c("red"),horiz=TRUE,cex=0.8)

hist(idadeMasc,breaks=30,main = "Frequencia de Obitos",
     xlab="Idades",ylab="",xlim=c(20,90),ylim=c(0,5),col="green")
legend("topright",inset=.02,title="Sexo",c("Masculino")
       ,fill = c("green"),horiz=TRUE,cex=0.8)


#ASSOCIAÇÃO DE OBITOS EM RELAÇÃO AO SEXO
comparacao <- table(tabelaProbabilidade$Sexo)
str(comparacao)

#DIFERENÇA HOMEM x MULHER

percent <- paste(c("Mulher: ","Homem: "),
round(prop.table(table(tabelaProbabilidade$Sexo)),digits =4)*100, "%")
print("FrequÃªncia relativa:")
print(percent)

# PLOTANDO O GRÁFICO DE PIZZA/SETORES
pie(comparacao,main="Situação de obitos em relação ao sexo",col=rainbow(2),radius=1,labels="")
legend("left", legend=percent,bty = "n",fill=rainbow(2),cex=0.9)

