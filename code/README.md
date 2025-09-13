#### PACOTES 
library(shiny)
library(bslib)
library(mirt)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(fmsb)
library(readxl)
library(openxlsx)


#### DADOS
setwd("F:\\dados")
dados <-data.frame(read_xlsx("simulado 4.xlsx"))

#### DADOS DO SIMULADO
gabarito <- c("D","D","D","E","B","C", "E","D","C","D")
#descritor <- c("D019","D019","D019","D019","D019","D062","D062","D062","D062","D062")
anul <- c()


#### CORREÇÃO DO GABARITO
#names(dados) <- c("aluno", "turma", "simulado", descritor)

for(i in 1:10){
  for(j in 1:nrow(dados)){
    if(dados[j, i+4]==gabarito[i]){dados[j, i+4] <- 1} else {dados[j, i+4] <- 0}
  }
}
for(i in 1:length(gabarito)){dados[,i+4] <- as.numeric(c(dados[,i+4]))}
desger <- sum(dados[,10:14])/(nrow(dados)*5)

T <- c("Geral",c(unique(dados$Turma)))

pdesc <- matrix(c(NA), ncol=length(gabarito), nrow=length(T))
colnames(pdesc) <- gabarito


for(j in 1:length(c("Geral",c(unique(dados$Turma))))){
  if(j==1){
    for(i in 1:length(gabarito)){pdesc[j, i] <- round(100*sum(as.numeric(c(dados[,i+4])))/nrow(dados),2)}
  } else {
    for(i in 1:length(gabarito)){pdesc[j, i] <- round(100*sum(as.numeric(c(dados[dados[,2]==T[j],i+4])))/nrow(dados[dados[,2]==T[j],]), 2)}
  }
}

rownames(pdesc) <- T
colnames(pdesc) <- c(1:length(gabarito))
pdesc[,2] <- c(unique(dados$simulado))
round(pdesc,0)






quest <- dados[,-c(1:4)]
model <- mirt(quest, 1, itemtype = "3PL")
modelo_coef <- coef(model, simplify = TRUE, IRTpars = TRUE)
modelo_coef

prof <- fscores(model, full.scores = TRUE)

pont <- expected.test(model, Theta=prof[,'F1'])
prof_padronizada <- 500*pont/length(gabarito)

data <- data.frame(turma=dados$Turma, Nome=dados$Nome, nota=prof_padronizada, acertos=apply(quest, 1, sum))

P2 <- plot(model, type = "trace")

P1<-ggplot(data, aes(x=turma, y=prof_padronizada, fill=turma)) +
  geom_boxplot()+
  xlab('Turma')+ylab('Nota Padronizada')+
  scale_fill_manual(values = c("#f08080", "#7b68ee", "#7ba05b", "#ff9bff")) +
  geom_boxplot(width=0.1) + theme_minimal()+
  theme(legend.position="none")


#### Padrão de proficiência
p <- c(250,300,350,500) #LP
p <- c(275,325,375,500) #MAT

pp <- matrix(c(NA), ncol=4, nrow=length(T))
colnames(pp) <- c("AB","B","P","A")
rownames(pp) <- T

for(j in 1:length(T)){
  if(j==1){
    Data <- data
    for(i in 1:4){
      if(i==1){
        pp[j, i] <- round(100*nrow(Data[Data$nota<p[i],])/nrow(Data),2)
      } else {
        pp[j, i] <- round(100*(nrow(Data[Data$nota<p[i],])-nrow(Data[Data$nota<p[i-1],]))/nrow(Data),2)
      }
  }
  } else {
    Data <- data[data$turma==T[j],]
    for(i in 1:4){
      if(i==1){
        pp[j, i] <- round(100*nrow(Data[Data$nota<p[i],])/nrow(Data),2)
      } else {
        pp[j, i] <- round(100*(nrow(Data[Data$nota<p[i],])-nrow(Data[Data$nota<p[i-1],]))/nrow(Data),2)
      }
    }
  }
}

round(pp,0)

write.xlsx(merge(dados, data[,-c(1)], by=c("Nome","Nome")), "Resultado S1 LP.xlsx")
