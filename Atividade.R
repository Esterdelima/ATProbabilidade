#criei tabela em R e li
filename <- "dados.csv"
tabelaProbabilidade <- read.csv(filename, header = TRUE, sep =',')

#Executei a leitura
str(tabelaProbabilidade)

#Montagem da tabela (agrupagem da tabela)
dataF <- data.frame(tabelaProbabilidade)

#associação da tabela com os dados
comparacao <- table(tabelaProbabilidade$Idade)
str(comparacao)

#ver o que fiz
print("Frequência absoluta:")
print(comparacao)

#histograma de frequencias de caso
hist(dataF$Idade,breaks=80,main = "Frequência de casos",
     xlab="Idades",ylab="",xlim=c(1,100),ylim=c(0,5),col="red")

#histograma de idadexcasosxsexo
idadeMasc <- dataF$Idade[which(dataF$Sexo=='Masculino')]

idadeFem <- dataF$Idade[which(dataF$Sexo=='Feminino')]

hist(dataF$Idade,breaks=80,main = "Frequência de casos",
     xlab="Idades",ylab="",xlim=c(20,90),ylim=c(0,3),col=rgb(1,0,0,0.5))

hist(idadeMasc,breaks=100,main = "Frequência de casos",
     xlab="Idades",ylab="",xlim=c(20,90),ylim=c(0,3),col=rgb(1,0,0,0.5))

hist(idadeFem,breaks=100,main = "Frequência de casos",
     xlab="Idades",ylab="",xlim=c(20,90),ylim=c(0,3),col=rgb(0,0,1,0.5),add=
       T)
legend("topright",inset=.02,title="Sexo",c("Masculino","feminino")
       ,fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),horiz=TRUE,cex=0.8)


#associação da tabela com dos dados com Sexo (questão 3)
comparacao <- table(tabelaProbabilidade$Sexo)
str(comparacao)

#diferença homemxmulher

percent <- paste(c("Mulher: ","Homem: "),
round(prop.table(table(tabelaProbabilidade$Sexo)),digits =4)*100, "%")
print("Frequência relativa:")
print(percent)

# PLOTANDO O GRÁFICO DE PIZZA/SETORES
pie(comparacao,main="Situação atual dos
casos",col=rainbow(2),radius=1,labels="")
legend("left", legend=percent,bty = "n",fill=rainbow(2),cex=1)






#Questão 1 e 2

metragem <- c( 48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41,
               55, 49, 61, 49, 49, 52, 55, 60, 52, 54, 57, 47, 66, 60, 53, 
               59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40)

dataF2 <- data.frame(metragem)

#questão 1)a
y<-1:45
y=y[-7]
metragem <- append(y,93)
Frequencia_absoluta<-table(metragem)
Frequencia_relativa<-round(Frequencia_absoluta/sum(Frequencia_absoluta)*
                             100,digits=2)
tabelafinal<-cbind(metragem,Frequencia_absoluta,Frequencia_relativa)

#adicionando os valores totais
total<-c("Total",sum(Frequencia_absoluta),sum(Frequencia_relativa))
tabelafinal<-rbind(tabelafinal,total)
write.table(tabelafinal,"dados-frequencia.txt",dec=".",sep =
              ",",quote=FALSE)




