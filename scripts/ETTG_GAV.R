
#Path
setwd("D:/Documents/Projects/CAGECE")

#libraries 
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

#dataset
ETRG_GAV = read_excel("Datasets/ETRG_GAV/ETA_GaV.xlsx",sheet = "data")
V_ETRG = read_excel("Datasets/ETRG_GAV/Vazao_ETRG.xlsx",sheet = "data")
Q_ETRG = read_excel("Datasets/ETRG_GAV/Quimico_ETRG.xlsx",sheet = "data")

#back-up
Gav.tb = ETRG_GAV

Gav.df = as.data.frame(ETRG_GAV)


# Data Exploration  -------------------------------------------------------

gav <-Gav.df 

str(gav)

gav$Amostragem = as.factor(gav$Amostragem)
gav$Abreviado =as.factor(gav$Abreviado)
gav$Parâmetro =as.factor(gav$Parâmetro) 
gav$Data_Hora = gav$Data
gav$Data = as.Date(gav$Data_Hora)

m = as.factor(month(gav$Data,label = F))
levels(m) = c("01","02","03","04","05","06",
              "07","08","09","10","11","12")

gav$Mes_Ano = as.factor(str_c(str_sub(gav$Data,
                                      end = 4),"-",m))
gav$Resultado1 = str_replace_all(gav$Resultado, '\\.|/|-', '')
gav$Resultado1 = str_replace_all(gav$Resultado1, ',', '.')
gav$Resultado1 = str_replace_all(gav$Resultado1, "Virtualmente ausentes","0")
gav$Resultado1 = str_replace_all(gav$Resultado1, "x10","E")
gav$Resultado1 = str_replace_all(gav$Resultado1, "<","")
gav$Resultado1 = str_replace_all(gav$Resultado1, ">","")
gav$Resultado1 = str_replace_all(gav$Resultado1, "Virtualmente presentes","1")
gav$Resultado1 = as.numeric(gav$Resultado1)
gav.data = gav[,c(1,2,9,4,3,5,10)]
str(gav.data)



# Data visualization -----------------------------------------------------

summary(gav.data$Abreviado)

gav.def = gav.data[gav.data$Abreviado=="Al"|
                     gav.data$Abreviado=="DQO",]
                     gav.data$Abreviado=="MF"|
                     gav.data$Abreviado=="pH"|
                     gav.data$Abreviado=="SS"|
                     gav.data$Abreviado=="SST"|
                     gav.data$Abreviado=="Temp",]

Lista_out = c("Ciano","Clo_a", "CT", "E_coli",
              "Feo_a","Fito","STX","OrtoP","MC")
Lista_in = c("Al","DQO", "MF", "pH",
             "SS","SST","Temp")


gav.def$Amostragem = as.factor(as.character(gav.def$Amostragem))
gav.def$Abreviado =  as.factor(as.character(gav.def$Abreviado))
gav.def$Parâmetro =  as.factor(as.character(gav.def$Parâmetro)) 
summary(gav.def)



View(gav.def[sort(gav.def$Abreviado),])

attach(gav.def)

ggplot(gav.def, aes(x = Amostragem,
                    y = Resultado1))+
  geom_boxplot()+
  facet_grid(Abreviado~Amostragem, scales = "free")


a$Abreviado =  as.factor(as.character(a$Abreviado))
a$Amostragem = as.factor(as.character(a$Amostragem))
a$Parâmetro =  as.factor(as.character(a$Parâmetro)) 
a$Data = as.Date(a$Data)

a = ((gav.def[gav.def$Abreviado=="SST",] &
                   gav.def$Amostragem=="APÓS A LAVAGEM DOS FILTROS",]))



#IMPUTANDO A MEDIA
fix(a)# Acrescentando uma observação em 2020-04
a=a[order(a$Data),]
a$Resultado2 = a$Resultado1
a[which(is.na(a$Resultado2)),8] = 
  mean(c(a[which(is.na(a$Resultado1))+1,7],
         a[which(is.na(a$Resultado1))-1,7]))
a$Tipo = "Observado"
a[which(is.na(a$Resultado1)),9] = "Imputado" 
a$Tipo = as.factor(a$Tipo)
summary(a)

write(a)

write.table((unique(a$Data)),
            file = "Outputs/DatasSST.txt",
            row.names = F,col.names = T)

sst.ts= ts(a$Resultado2, frequency = 12, 
           end =c(2022, 4) )


print(sst.ts)
plot(sst.ts)


dygraph(
  data = sst.ts,
  main = "Sólidos suspensos totais",
  xlab = "",
  ylab = "mg/L")

decomposeAditiva <- decompose(
  x = sst.ts,
  type = "additive"
)
plot(
  decomposeAditiva
)

# Modelagem multiplicativa
decomposeMultiplicacao <- decompose(
  x = sst.ts, 
  type = "multiplicative"
)   

# Grafico decomposicao da modelagem multiplicativa
plot(
  decomposeMultiplicacao
)

start_Date = a$Data[1]
end_Date = a$Data[34]
A = ggplot(a, aes(x = Data,y = Resultado2)) +
  geom_line() +
  geom_point(aes(color = Tipo), size = 2)+
  scale_x_date(limits = c(start_Date, end_Date))+
 # geom_hline(yintercept =1, linetype = 2)+
  theme_cowplot()+
  theme(legend.title=element_blank(),#element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  
  xlab("") +
  ylab(expression(paste("SST (mg",L)^-1,")"));A


#Vol Mensal

v = as.data.frame(V_ETRG)
v$Ano_Mes = as.factor(v$Ano_Mes)

lista_mes = levels(v$Ano_Mes)

v$TotalDia = (v$ASCENSIONAL+v$SUPERFICIAL)/30

CargaSST = data.frame()
for (i in 1:29) {
CargaSST[i,"Mês"] = lista_mes[i]
CargaSST[i,"Ton/Dia"] = a[a$Mes_Ano==lista_mes[i],8]*
    v[v$Ano_Mes==lista_mes[i],5]*1E-6
  

}

v$Data = as.Date(str_c(v$Ano_Mes,"-01"))

CargaSST$Data = as.Date(str_c(CargaSST$Mês,"-01"))

Vazão = ggplot(v, aes(x = Data,
                         y = TotalDia)) +
  geom_line() +
  geom_point(size = 2)+
  theme_cowplot()+  xlab("") +
  ylab(expression(paste("M³ ",Dia)^-1,")"));Vazão
CargaSST$Lavagem = "Volume Mensal"

ggsave2("Graph/ETRG_GAV/Vazão.png",Vazão,
        width = 9,height = 4,units = "in", dpi = 600, limitsize =  F)
  

ts.carga = ts(CargaSST$`Ton/Dia`,
              start = c(2019,12), 
              frequency = 12)

ts.v = ts(v$TotalDia,start = c(2019,12), 
              frequency = 12)


plot(decompose(ts.v))
plot(decompose(ts.carga))



B = ggplot(CargaSST, aes(x = Lavagem,
                         y = `Ton/Dia`)) +
 geom_boxplot()+
  theme_cowplot()+  xlab("") +
  ylab(expression(paste("Tonelada de SST ",Dia)^-1,")"));B



#Quimicos

q = as.data.frame(Q_ETRG)
q$`Ano-Mês` = as.factor(q$`Ano-Mês`)

lista_mes = levels(v$Ano_Mes)

q$TotalDia = round(((.8*q$PAC+q$POLIMERO)/30)/1000,2)

q$DATA = as.Date(q$DATA)

C = ggplot(q, aes(x = DATA,
                         y = TotalDia)) +
  geom_line() +
  geom_point(size = 2)+
  theme_cowplot()+  xlab("") +
  ylab(expression(paste("Tonelada de Quimico ",Dia)^-1,")"));

q$Lavagem = "Volume Mensal"

D = ggplot(q, aes(x = Lavagem,
                         y = TotalDia)) +
  geom_boxplot()+
  theme_cowplot()+  xlab("") +
  ylab(expression(paste("Tonelada de Quimico ",Dia)^-1,")"));D



# SS ----------------------------------------------------------------------

b = ((gav.def[gav.def$Abreviado=="SS" &
                gav.def$Amostragem=="APÓS A LAVAGEM DOS FILTROS",]))

summary(b)
table(b$Mes_Ano)
write.table((unique(b$Data)),
            file = "Outputs/Datas.txt",
            row.names = F,col.names = T)

#IMPUTANDO A MEDIA
fix(a)# Acrescentando uma observação em 2020-04
a=a[order(a$Data),]
a$Resultado2 = a$Resultado1
a[which(is.na(a$Resultado2)),8] = 
  mean(c(a[which(is.na(a$Resultado1))+1,7],
         a[which(is.na(a$Resultado1))-1,7]))
a$Tipo = "Observado"
a[which(is.na(a$Resultado1)),9] = "Imputado" 
a$Tipo = as.factor(a$Tipo)
summary(a)

sst.ts= ts(a$Resultado2, frequency = 12, 
           end =c(2022, 4) )


print(sst.ts)
plot(sst.ts)


dygraph(
  data = sst.ts,
  main = "Sólidos suspensos totais",
  xlab = "",
  ylab = "mg/L")

decomposeAditiva <- decompose(
  x = sst.ts,
  type = "additive"
)
plot(
  decomposeAditiva
)

# Modelagem multiplicativa
decomposeMultiplicacao <- decompose(
  x = sst.ts, 
  type = "multiplicative"
)   

# Grafico decomposicao da modelagem multiplicativa
plot(
  decomposeMultiplicacao
)

start_Date = a$Data[1]
end_Date = a$Data[34]
A = ggplot(a, aes(x = Data,y = Resultado2)) +
  geom_line() +
  geom_point(aes(color = Tipo), size = 2)+
  scale_x_date(limits = c(start_Date, end_Date))+
  # geom_hline(yintercept =1, linetype = 2)+
  theme_cowplot()+
  theme(legend.title=element_blank(),#element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  
  xlab("") +
  ylab(expression(paste("SST (mg",L)^-1,")"));A

a = ggseasonplot(sst.ts, polar = F)+
  ggtitle("")+
  geom_point(size = 2)+
  theme_bw()+
  #geom_vline(xintercept = c(.4,.7),
  #           linetype = 2)+
  facet_grid(~year, scales = "free")+
  theme(axis.text.x = element_text(size = rel(.75),
                                   angle = 90))+
  guides(colour = "none")


