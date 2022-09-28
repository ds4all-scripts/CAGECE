# Path, libraries and datasets --------------------------------------------

setwd("D:/Documents/Projects/CAGECE")

library(ggplot2)
library(reshape)
library(tidyverse)
library(cowplot)
library(readxl)
library(stringr)
library(lubridate)
library(dygraphs)
library(reshape)
library(dplyr)
library(lubridate)
library(stats)
library(tseries)
library(rugarch)
library(fpp)
library(forecast)
library(car)
library(nortest)
require(graphics)
library(mFilter)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(randomForest)
library(readxl)



hb.2019 <- read_excel("Datasets/HidroBio/HB_2019.xlsx")
hb.2020 <- read_excel("Datasets/HidroBio/HB_2020.xlsx")
hb.2021 <- read_excel("Datasets/HidroBio/HB_2021.xlsx")


# Functions ---------------------------------------------------------------

Valor_Correto = function(A){
  A$Valor_Correto = str_replace_all(A[,NCOL(A)], '\\.|/|-', '')
  A$Valor_Correto = str_replace_all(A$Valor_Correto, ',', '.')
  A$Valor_Correto = str_replace_all(A$Valor_Correto, "x10","E")
  A$Valor_Correto = str_replace_all(A$Valor_Correto, "<","")
  A$Valor_Correto = str_replace_all(A$Valor_Correto, ">","")
  A$Valor_Correto = as.numeric(A$Valor_Correto)
  return(A)
}


# 2019 --------------------------------------------------------------------

head(hb.2019)
d1= hb.2019[,1:8]
for (i in 2:NROW(d1)) {
  for (j in 1:8) {
    if (is.na(d1[i, j])) {
      d1[i, j] = d1[i - 1, j]
    }
  }
}

D1=cbind(d1,hb.2019[,8:16])

ciano = Valor_Correto(na.omit(D1[,c(1:7,11)]))
CYN   = Valor_Correto(na.omit(D1[,c(1:7,12)]))
CHla  = Valor_Correto(na.omit(D1[,c(1:7,13)]))
Feo   = Valor_Correto(na.omit(D1[,c(1:7,14)]))
Fito  = Valor_Correto(na.omit(D1[,c(1:7,15)]))
MC    = Valor_Correto(na.omit(D1[,c(1:7,16)]))
STX   = Valor_Correto(na.omit(D1[,c(1:7,17)]))

# 2020 --------------------------------------------------------------------

head(hb.2020)
d2= hb.2020[,1:8]
for (i in 2:NROW(d2)) {
  for (j in 1:8) {
    if (is.na(d2[i, j])) {
      d2[i, j] = d2[i - 1, j]
    }
  }
}

D2=cbind(d2,hb.2020[,8:16])

ciano.1 = Valor_Correto(na.omit(D2[,c(1:7,11)]))
CYN.1   = Valor_Correto(na.omit(D2[,c(1:7,12)]))
CHla.1  = Valor_Correto(na.omit(D2[,c(1:7,13)]))
Feo.1   = Valor_Correto(na.omit(D2[,c(1:7,14)]))
Fito.1  = Valor_Correto(na.omit(D2[,c(1:7,15)]))
MC.1    = Valor_Correto(na.omit(D2[,c(1:7,16)]))
STX.1   = Valor_Correto(na.omit(D2[,c(1:7,17)]))


# 2021 --------------------------------------------------------------------

head(hb.2021)
d3= hb.2021[,1:8]
for (i in 2:NROW(d3)) {
  for (j in 1:8) {
    if (is.na(d3[i, j])) {
      d3[i, j] = d3[i - 1, j]
    }
  }
}

D3=cbind(d3,hb.2021[,8:16])

ciano.2 = Valor_Correto(na.omit(D3[,c(1:7,11)]))
CYN.2   = Valor_Correto(na.omit(D3[,c(1:7,12)]))
CHla.2  = Valor_Correto(na.omit(D3[,c(1:7,13)]))
Feo.2   = Valor_Correto(na.omit(D3[,c(1:7,14)]))
Fito.2  = Valor_Correto(na.omit(D3[,c(1:7,15)]))
MC.2    = Valor_Correto(na.omit(D3[,c(1:7,16)]))
STX.2   = Valor_Correto(na.omit(D3[,c(1:7,17)]))



# Intervalo 2019-2021 -----------------------------------------------------

chla.t = rbind(CHla,CHla.1,CHla.2)
feo.t = rbind(Feo,Feo.1,Feo.2)
Fit.t = rbind(Fito,Fito.1,Fito.2)
mc.t = rbind(MC,MC.1,MC.2)
stx.t = rbind(STX,STX.1,STX.2)
ciano.t = rbind(ciano,ciano.1,ciano.2)
cyn.t = rbind(CYN,CYN.1,CYN.2)

ciano.t$Parametro = colnames(ciano.t)[8]
cyn.t$Parametro = colnames(cyn.t)[8]
chla.t$Parametro = colnames(chla.t)[8]
feo.t$Parametro = colnames(feo.t)[8]
Fit.t$Parametro = colnames(Fit.t)[8]
mc.t$Parametro = colnames(mc.t)[8]
stx.t$Parametro = colnames(stx.t)[8]


colnames(ciano.t)[8] = "Valor_Base"
colnames(cyn.t)[8] = "Valor_Base"
colnames(chla.t)[8] = "Valor_Base"
colnames(feo.t)[8] = "Valor_Base"
colnames(Fit.t)[8] = "Valor_Base"
colnames(mc.t)[8] = "Valor_Base"
colnames(stx.t)[8] = "Valor_Base"

Total_Geral = rbind(ciano.t,cyn.t,chla.t,feo.t,Fit.t,mc.t,stx.t)


# Definições  ---------------------------------------------------

Total_Geral$Cliente = as.factor(Total_Geral$Cliente) 
Total_Geral$Sistema = as.factor(Total_Geral$Sistema)
Total_Geral$Localidade = as.factor(Total_Geral$Localidade)
Total_Geral$Parametro = as.factor(Total_Geral$Parametro)
Total_Geral$`Ponto de coleta` = as.factor(Total_Geral$`Ponto de coleta`)
Total_Geral$`Natureza da amostra` = as.factor(Total_Geral$`Natureza da amostra`)
Total_Geral$`Ponto de amostragem` = as.factor(Total_Geral$`Ponto de amostragem`)
Total_Geral$`Data da coleta` = as.factor(Total_Geral$`Data da coleta`)



str(Total_Geral)

write.csv(Total_Geral,"Datasets/HidroBio/Total_2019_2021_1.csv")


# Análises de cada variável  ----------------------------------------------

#Cliente: unidades de Negócio
levels(Total_Geral$Cliente)

#Sistema: difere da localidade, melhor utilizar localidade
levels(Total_Geral$Sistema)

#localidade: utilizar ao invés de Sistema 
levels(Total_Geral$Localidade)

#Parâmetros analisados
levels(Total_Geral$Parametro)

#Qual a definiçao e Como agrupar esses pontos ?
levels(Total_Geral$`Ponto de coleta`)

#Qual a definiçao e Como agrupar esses pontos ?
levels(Total_Geral$`Natureza da amostra`)



#A data pode ser utilizada como fator ou POSIXct
levels(Total_Geral$`Data da coleta`)
as.POSIXct.Date(Total_Geral$`Data da coleta`)

summary(Total_Geral)


# Manancial superficial ----------------------------------------------------


Super = Total_Geral[Total_Geral$`Natureza da amostra`=="MANANCIAL SUPERFICIAL",]

Super$Cliente = as.factor(as.character(Super$Cliente)) 
Super$Sistema = as.factor(as.character(Super$Sistema)) 
Super$Localidade = as.factor(as.character(Super$Localidade)) 
Super$Parametro = as.factor(as.character(Super$Parametro)) 
Super$`Ponto de coleta` = as.factor(as.character(Super$`Ponto de coleta`))
Super$`Natureza da amostra` = as.factor(as.character(Super$`Natureza da amostra`))
Super$`Ponto de amostragem` = as.factor(as.character(Super$`Ponto de amostragem`))
Super$`Data da coleta` = as.factor(as.character(Super$`Data da coleta`))

table(Super$`Ponto de amostragem`)
table(Super$`Ponto de coleta`)
sort(table(Super$Localidade))
sort(table(Super$Sistema))



# Dataset para figura -----------------------------------------------------


N_obs = read_xlsx("Datasets/HidroBio/Municípios_CE.xlsx",
                  sheet = "Planilha1",col_names = F )

freq = as.data.frame(table(Super$Localidade))

Loc_Freq = data.frame("Localiade" = N_obs$...1,
                      "Freq" = 0)

Loc_Freq$Localiade = as.character(str_to_upper(Loc_Freq$Localiade))

for (i in 1:121) {
  if (i != 98) {
    Loc_Freq[Loc_Freq$Localiade == freq$Var1[i], 2] = freq[i, 2]
  }
}


write.table(table(Super$Localidade),"Datasets/HidroBio/localidades.txt")


freq$Marcador = 0

for (i in 1:184) {
  if (dim(freq[freq$Var1 == Loc_Freq[i,1],])[1]!=0) {
    freq[freq$Var1 == Loc_Freq[i,1],3]=1
  }
}

(freq[freq[3]<1,])

freq$Muni = as.character(freq$Var1)

# Ajustando 
freq[freq$Marcador<1,][1,4] = "Araripe"

freq[freq$Marcador<1,][2,4] = "Maranguape"

freq[freq$Marcador<1,][3,4] = "Lavras da Mangabeira"

freq[freq$Marcador<1,][4,4] = "Sobral"

freq[freq$Marcador<1,][5,4] = "Ibiapina"

freq[freq$Marcador<1,][6,4] = "Russas"

freq[freq$Marcador<1,][7,4] = "Miraíma"

freq[freq$Marcador<1,][8,4] = "Caucaia"

freq[freq$Marcador<1,][9,4] = "Tururu"

freq[freq$Marcador<1,][10,4] = "Itapipoca"

freq[freq$Marcador<1,][11,4] = "Iracema"

freq[freq$Marcador<1,][12,4] = "Orós"

freq[freq$Marcador<1,][13,4] = "Barro"

freq[freq$Marcador<1,][14,4] = "Aracoiaba"

freq[freq$Marcador<1,][15,4] = "Aurora"

freq[freq$Marcador<1,][16,4] = "Massapê"

freq[freq$Marcador<1,][17,4] = "Hidrolândia"

freq[freq$Marcador<1,][18,4] = "Maranguape"

freq[freq$Marcador<1,][19,4] = "Sobral"

freq[freq$Marcador<1,][20,4] = "Croatá"

freq[freq$Marcador<1,][21,4] = "Pires Ferreira"

freq[freq$Marcador<1,][22,4] = "São Gonçalo do Amarante"

freq[freq$Marcador<1,][23,4] = "Santa Quitéria"

freq[freq$Marcador<1,][24,4] = "Lavras da Mangabeira"

freq[freq$Marcador<1,][25,4] = "Santa Quitéria"

freq[freq$Marcador<1,][26,4] = "Umirim"

freq[freq$Marcador<1,][27,4] = "Maranguape"

freq[freq$Marcador<1,][28,4] = "Apuiarés"

freq[freq$Marcador<1,][29,4] = "Beberibe"

freq[freq$Marcador<1,][30,4] = "Caucaia"

freq[freq$Marcador<1,][31,4] = "Tamboril"

freq[freq$Marcador<1,][32,4] = "Aquiraz"

freq[freq$Marcador<1,][33,4] = "Chorozinho"

freq[freq$Marcador<1,][34,4] = "Coreaú"

freq[freq$Marcador<1,][35,4] = "Moraújo"

freq$Muni = as.character(str_to_upper(freq$Muni))


Freq_Def = as.data.frame(tapply(freq$Freq, freq$Muni, sum))

Freq_Def$Loc = row.names(Freq_Def)

Freq_Def = Freq_Def[,2:1] 
row.names(Freq_Def) = 1:NROW(Freq_Def)
colnames(Freq_Def) = c("Loc","Freq")


#definitivo:

for (i in 1:184) {
  if (dim(freq[freq$Var1 == Loc_Freq[i,1],])[1]!=0) {
    freq[freq$Var1 == Loc_Freq[i,1],3]=1
  }
}


for (i in 1:100) {
  Loc_Freq[Loc_Freq$Localiade == Freq_Def$Loc[i],2] = freq[i, 2]
}


write.table(Loc_Freq, "Datasets/HidroBio/local_Freq.txt")
