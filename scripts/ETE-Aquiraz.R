
# Path,  Libraries,  and Datasets  ---------------------------------------------

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

#dataset
ETE_AQUIRAZ = read_excel("Datasets/ETE_AQUIRAZ.xlsx")

#back-up
Aq.tb = ETE_AQUIRAZ

Aq.df = as.data.frame(ETE_AQUIRAZ)

# Exploratory Data Analysis ----------------------------------------------------

# formatting the database

data=Aq.df
for (i in c(1,2,3, 6,8)) {
  data[,i] = as.factor(data[,i])
}

data$Resultado1 = str_replace_all(data$Resultado, '\\.|/|-', '')
data$Resultado1 = str_replace_all(data$Resultado1, ',', '.')
data$Resultado3 = str_replace_all(data$Resultado1, "x10","E")
data$Resultado3 = str_replace_all(data$Resultado3, "<","")
data$Resultado3 = str_replace_all(data$Resultado3, ">","")
data$Resultado3 = str_replace_all(data$Resultado3, "Virtualmente presentes","1")
data$Resultado3 = str_replace_all(data$Resultado3, "Virtualmente ausentes","0")
data$Resultado4 = as.numeric(data$Resultado3)
options(scipen = 999)
data$Data_Coleta = as.Date(data$Data_coleta)
data$Data_hora_coleta = as.POSIXct(data$Hora_Coleta)
data.total = data[,c(1,2,3,8,7,12,13,6,11)]


# Correction of dataset
View(data.total[data.total$Ponto == levels(data.total$Ponto)[1],])
View(data.total[data.total$Ponto == levels(data.total$Ponto)[2],]) #Retirar
View(data.total[data.total$Ponto == levels(data.total$Ponto)[3],]) #165 a 173 tratado para tratamento
View(data.total[data.total$Ponto == levels(data.total$Ponto)[4],]) #Retirar
View(data.total[data.total$Ponto == levels(data.total$Ponto)[5],]) #174 a 182 tratado para tratamento
View(data.total[data.total$Ponto == levels(data.total$Ponto)[6],]) #183 a 192 tratado para tratamento
View(data.total[data.total$Ponto == levels(data.total$Ponto)[7],]) #183 a 192 tratado para tratamento
View(data.total[165:192,])

levels(data.total$Tipo_Amostra) = c("Bruto", "Tratado", "Tratamento")
data.total$Tipo_Amostra = factor(data.total$Tipo_Amostra,
                                 levels = c("Bruto","Tratamento" , "Tratado"))


#1st 
data.total$Amostragrem = data.total$Tipo_Amostra
data.total$Amostragrem[165:192] = "Tratamento"

#2nd
View(data.total[c(959:964,809,810),]) 
data.bck = data.total
data.total = data.bck[-c(959:964,809,810),]
data.total$Amostragrem
data.total$Coleta = data.total$Ponto
levels(data.total$Coleta) = c("Preliminar","ENTRADA_FAC", 
                             "Anaeróbia", "SAIDA_ETE",  
                             "Facultativa",  "1ª Maturação", 
                             "2ª Maturação")
data.total$Coleta = as.factor(as.character(data.total$Coleta))
data.total$Coleta = factor(data.total$Coleta , 
                           levels =  c("Preliminar",
                                       "Anaeróbia", 
                                       "Facultativa",
                                       "1ª Maturação", 
                                       "2ª Maturação"))
#3nd
levels(data.total$Parametro) = c( "Amônia(como N) - mg/L"      ,"Cálcio - mg/L"             ,"Cianobactérias - - cel/mL",
                                  "Cloreto - mg/L"       ,"Coliformes Totais - NMP/100 mL"    ,"Condutividade - uS/cm" ,
                                  "Cor Aparente - uH"      ,"Cor Verdadeira -uH"    ,"DBO - mg/L"           ,
                                  "DBO filtrada - mg/L" , "DQO - mg/L"           , "DQO Filtrada - mg/L" , 
                                  "Escherichlia coli- NMP/100 mL"       , "Potássio- mg/L"    , "Materiais Flutuantes"       , 
                                  "Sódio - mg/L"           , "Nitrato (como N) - mg/L"     , "Nitrito (como N) - mg/L"    , 
                                  "Oxigênio Dissolvido - mg/L"           , "Óleos e Graxas - mg/L"       , "pH"           , 
                                  "Fósfoto Total - mg/L"       , "Sólidos Sedimemtáveis - mL/L", "Sólidos Suspensos Totais - mg/L"  , 
                                  "Sulfeto - mg/L"      , "Temperatura - ºC"  )

lista = c( "Coliformes Totais - NMP/100 mL",
           "DQO - mg/L",
           "Escherichlia coli- NMP/100 mL",
           "pH", 
           "Sólidos Suspensos Totais - mg/L")
lista2 = c("Anaeróbia", "Facultativa",
           "1ª Maturação","2ª Maturação")

summary(data.total)

#  Raw Wastewater -----------------------------------------------------------
Lista_Total = Lista_Max = list()
for (j in lista) {
  x = (data.total[data.total$Parametro == j,])
  for (i in levels(data.total$Coleta)) {  
    cat(j,i,"\n")
    # choose the parameter
    x1 = x[x$Coleta == i, ] 
    # Create month-year values column
    m = as.factor(month(x1$Data_Coleta,label = F))
    levels(m) = c("01","02","03","04","05","06","07","08","09","10","11","12")
    x1$Mes_Ano= as.factor(str_c(
      str_sub(x1$Data_Coleta,end = 4),"-",m))
   
    #Create the data frame of max values
    x2 = as.data.frame(tapply(x1$Resultado4,
                              x1$Mes_Ano, 
                              mean))

    cat("Aqui ok, \n")
    #Create month-year values column for the graph
    x2$Mes_Ano = row.names(x2)
    #Rename names of columns
    colnames(x2) = c("Resultado", "Mes_Ano")
    #sorted dataset
    x2 = x2[order(x2$Mes_Ano), ]
    Lista_Max[[i]] = x2
  }
  Lista_Total[[j]] = Lista_Max
}

Lista_Total

#Graphics 

setwd("D:/Documents/Projects/CAGECE/Graph/")
Und = c("NMP/100 mL\n","mg/L","NMP/100 mL"," ","mg/L")

for (j in levels(data.total$Tipo_Amostra)) {
  x = (data.total[data.total$Tipo_Amostra == j ,])
   for (i in 1:length(lista)) {
    png(filename = paste(j,
              levels(data.bruto$Parametro)[i],
                       "- Maximas.png"),
        width = 10,
        height = 6,
        units = "in",
        res = 500)
     cat(j,levels(data.bruto$Parametro)[i],"###################","\n")
    par(mar = c(7, 8, 4, 3))
    lab = Lista_Total[[j]][[levels(data.bruto$Parametro)[i]]][2][[1]]
    print(lab)
    a = Lista_Total[[j]][[levels(data.bruto$Parametro)[i]]][1][[1]]
    print(a)
    plot(a,
         type = "o",
         ylab = paste(Und[i], "\n", "\n"),
         xaxt = "n",
         xlab = "",
         main = levels(data.bruto$Parametro)[i],
         las = 2)
    axis( 1,
          at = 1:NROW(lab),
          labels = lab,
          las  = 2,
          hadj = 1)    
    dev.off()
    cat("###################","\n")
  }
}
setwd("D:/Documents/Projects/CAGECE")


# GGPLOT ------------------------------------------------------------------


dt = data.total[data.total$Parametro == lista[1]
               |data.total$Parametro == lista[2]
               |data.total$Parametro == lista[3]
               |data.total$Parametro == lista[4]
               |data.total$Parametro == lista[5]
                ,]

#Tipo de Amostra

A = ggplot(dt, aes(x = Tipo_Amostra,y = Resultado4))+
  geom_boxplot()+
 facet_grid(Parametro~.,scales = "free");
A = A+theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  ylab("")+xlab("")
B = ggplot(dt, aes(x = Tipo_Amostra,y = Resultado4))+
    geom_boxplot()+
    facet_grid(Parametro~.,scales = "free")
B = B+theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape= 8, size=2)+
  ylab("")+xlab("")
C = plot_grid(A,B)
ggsave2(filename = "Graph/Diag.png",plot = C,device ="png",
        width = 12,height = 8,units = "in",dpi = 600,
        limitsize = F)

for (i in 1:5) {
  cat("\n","\n","Analisando = ",lista[i],"\n")
  
  a= pairwise.wilcox.test(dt$Resultado4[dt$Parametro==lista[i]],
                       dt$Tipo_Amostra[dt$Parametro==lista[i]],
                       p.adjust.method ="bonferroni",paired = F,
                       alternative = "t" )
  print(a$p.value)
  cat("\n","\n","####################################################","\n")
}

#Local de amostragem

A = ggplot(dt, aes(x = Coleta,y = Resultado4))+
  geom_boxplot()+
  facet_grid(Parametro~Amostragrem,scales = "free")+
  stat_summary(fun=mean, geom="point", shape= 8, size=2)+
  ylab("")+xlab("")+theme_bw();A

ggsave2(filename = "Graph/Diag2.png",plot = A,device ="png",
        width = 8,height = 10,units = "in",dpi = 2000,
        limitsize = F)

for (i in 1:5) {
  cat("\n","\n","Analisando = ",lista[i],"\n")
  
  a= pairwise.wilcox.test(dt$Resultado4[dt$Parametro==lista[i]],
                          dt$Coleta[dt$Parametro==lista[i]],
                          p.adjust.method ="bonferroni",paired = F,
                          alternative = "t" )
  print(a$p.value)
  cat("\n","\n","####################################################","\n")
}


# Efficiency ---------------------------------------------------------------

a=b=matrix(nrow = 5,ncol = 1)

for (i in 1:5) {
 a = as.matrix(tapply(dt$Resultado4[dt$Parametro==lista[i]],
       dt$Coleta[dt$Parametro==lista[i]], median))
colnames(a) = lista[i]

b = cbind(a,b) 
}
#Med = as.data.frame(b[,-6])
median = as.data.frame(b[,-6])
#sd = as.data.frame(b[,-6])

# Mean efficiency 
Ef = cbind(
t(round((((Med[1,]-Med[2,])/Med[1,])*100),3)),
t(round((((Med[1,]-Med[3,])/Med[1,])*100),3)),
t(round((((Med[1,]-Med[4,])/Med[1,])*100),3)),
t(round((((Med[1,]-Med[5,])/Med[1,])*100),3)))
colnames(Ef) = c("Anaeróbia", "Facultativa","1ª Maturação","2ª Maturação")
write.table(Ef,file = "Graph/Eficiencia.txt",row.names = T,col.names = T)


Ef_median = cbind(
  t(round((((median[1,]-median[2,])/median[1,])*100),3)),
  t(round((((median[1,]-median[3,])/median[1,])*100),3)),
  t(round((((median[1,]-median[4,])/median[1,])*100),3)),
  t(round((((median[1,]-median[5,])/median[1,])*100),3)))
colnames(Ef_median) = c("Anaeróbia", "Facultativa","1ª Maturação","2ª Maturação")
write.table(Ef_median,file = "Graph/Eficiencia_MEDIANA.txt",row.names = T,col.names = T)





# Efficiency(t) -------------------------------------------------------
x = (data.total[data.total$Parametro == j,])

head(y,n = 40)
y = x[,c(6,8:11)]
m = as.factor(month(y$Data_Coleta,label = F))
levels(m) = c("01","02","03","04","05","06",
              "07","08","09","10","11","12")
y$M_A = as.factor(str_c(str_sub(y$Data_Coleta,end = 4),"-",m))
 
x4 = x2 = x3 = data.frame("Resultado"=NA,
                          "Mes_Ano"=NA,
                          "Coleta" = NA,
                          "Parametro" = NA)
for (j in lista) {
  y = (data.total[data.total$Parametro == j,c(6,8:11)])
  m = as.factor(month(y$Data_Coleta,label = F))
  levels(m) = c("01","02","03","04","05","06",
                "07","08","09","10","11","12")
  y$M_A = as.factor(str_c(str_sub(y$Data_Coleta,end = 4),"-",m))
  cat("ok")
  for (i in levels(y$Coleta)) {
    z = (y[y$Coleta == i, ])
    x2 = as.data.frame(tapply(z$Resultado4,
                              z$M_A,
                              mean))
    cat("ok")
    #Create month-year values column for the graph
    x2$Mes_Ano = row.names(x2)
    x2$Coleta = i
    x2$Parametro = j
    #Rename names of columns
    colnames(x2) = c("Resultado", "Mes_Ano", "Coleta","Parametro")
    rownames(x2) = 1:NROW(x2)
    #sorted dataset
    x2 = x2[order(x2$Mes_Ano),]
    x3 = rbind(x3, x2)
 
  }
}
    x3 = x3[-1,]
    x3$Mes_Ano = as.factor(x3$Mes_Ano)  
    x3$Coleta = as.factor(x3$Coleta) 
    x3$Coleta = factor(x3$Coleta,levels = c("Preliminar",
                                            "Anaeróbia",
                                            "Facultativa",
                                            "1ª Maturação",
                                            "2ª Maturação"))
    x3$Parametro = as.factor(x3$Parametro)

eff = data.frame()
list_eff = list()
for (i in lista[c(2:5)]) {
  for (j in levels(x3$Mes_Ano)) {
    eff[j,"Data"] = j
    for (k in 2:5) {
     XX=( round(((x3[x3$Mes_Ano==j & 
                       x3$Parametro==i & 
                         x3$Coleta == levels(x3$Coleta)[1],]-
              x3[x3$Mes_Ano==j & x3$Parametro==i &
                   x3$Coleta==levels(x3$Coleta)[k] ,1])/
                x3[x3$Mes_Ano==j & x3$Parametro==i & 
                     x3$Coleta == levels(x3$Coleta)[1],1])*100,4))
     eff[j,levels(x3$Coleta)[k]]= ifelse(XX>100 |
                                           XX<0,NA,XX)
   list_eff[[i]] = eff
  }
  } 

}
#bkp = list_eff
#Eficiencia Máx e Mínima 
for (j in lista) {
  for (i in lista2) {
    MaX=which.max(list_eff[[j]][[i]])
    MiN=which.min(list_eff[[j]][[i]])
    m = round(list_eff[[j]][MiN,i],3)
    M = round(list_eff[[j]][MaX,i],3)
    Ef_MaxMin [i,j] = paste0("[",m," - ",M,"]")
    
  }
}

write.table(t(Ef_MaxMin),
            file = "Outputs/EF_Min_Max.txt",
            col.names = T,row.names = T)

CT = as.data.frame(list_eff[["Coliformes Totais - NMP/100 mL"]])
fix(CT)
png(filename = "Graph/Serie_Ef_CT.png",
    width = 10,
    height =8,
    units = "in",
    res = 500)
par(mfrow = c(2,2))
plot(abs(CT$Anaeróbia),type = "o",
     ylim=c(0,100),ylab = "Eficiencia de Remoção (%)",
      xlab = "",
     xaxt = "n",main = "Lagoa Anaeróbia")
axis( 1,
      at = 1:NROW(CT$Data),
      labels =CT$Data,
      las  = 2,
      hadj = 1)
abline(h = c(90,99), lty = 3)

arrows(x0 = 3, y0 = 90, 
       x1 = 3, y1 = 99, 
       length = .1,code = 3,
       angle = 20)

text(1.5,95,"LF",)

plot(abs(CT$Facultativa),type = "o",
     ylim=c(0,100),ylab = "",
     xlab = "",
     xaxt = "n",main = "Lagoa Facultativa")
axis( 1,
      at = 1:NROW(CT$Data),
      labels =CT$Data,
      las  = 2,
      hadj = 1)
abline(h = c(90,99), lty = 3)

arrows(x0 = 3, y0 = 90, 
       x1 = 3, y1 = 99, 
       length = .1,code = 3,
       angle = 20)

text(1.5,95,"LF",)

plot(abs(CT$`1ª Maturação`),type = "o",
     ylim=c(0,100),ylab = "Eficiencia de Remoção (%)",
     xlab = "",
     xaxt = "n",main = "1ªLagoa de Maturação")
axis( 1,
      at = 1:NROW(CT$Data),
      labels =CT$Data,
      las  = 2,
      hadj = 1)
abline(h = c(90,99), lty = 3)

arrows(x0 = 3, y0 = 90, 
       x1 = 3, y1 = 99, 
       length = .1,code = 3,
       angle = 20)

text(1.5,95,"LF",)

plot(abs(CT$`2ª Maturação`),type = "o",
     ylim=c(0,100),ylab = "",
     xlab = "",
     xaxt = "n",main = "2ªLagoa de Maturação")
axis( 1,
      at = 1:NROW(CT$Data),
      labels =CT$Data,
      las  = 2,
      hadj = 1)
abline(h = c(90,99), lty = 3)

arrows(x0 = 3, y0 = 90, 
       x1 = 3, y1 = 99, 
       length = .1,code = 3,
       angle = 20)

text(1.5,95,"LF",)
dev.off()
lista2 = c("Anaeróbia", "Facultativa",
           "1ª Maturação","2ª Maturação")
Ef_MaxMin = data.frame()


#  COEMA  -----------------------------------------------------------------

data.tratado = data.total[data.total$Amostragrem =="Tratado",]

data.tratado = data.tratado[order(data.tratado$Parametro),]
str(data.tratado)
data.tratado$Coleta = as.factor(as.character(data.tratado$Coleta))
data.tratado$Amostragrem = as.factor(as.character(data.tratado$Amostragrem))     
data.tratado$Parametro = as.factor(as.character(data.tratado$Parametro))       
d.t = data.tratado[,c(6,8,9)];d.t


lista3 = levels(d.t$Parametro)[c(19, 25, 22, 11, 17, 24,12,23)]
d.t1 = d.t[d.t$Parametro==lista3[1]|
           d.t$Parametro==lista3[2]|
           d.t$Parametro==lista3[3]|
           d.t$Parametro==lista3[4]|
           d.t$Parametro==lista3[5]|
           d.t$Parametro==lista3[6]|
           d.t$Parametro==lista3[7]|
           d.t$Parametro==lista3[8],]
tapply(d.t1$Resultado4, d.t1$Parametro, mean)
d.t1$Parametro = as.factor(as.character(d.t1$Parametro))
summary(d.t1)
coema =c(5,9, )

ph =  ggplot(d.t1[d.t1$Parametro==lista3[1],],
       aes(x = Parametro,
          y = Resultado4))+
      geom_boxplot()+
      geom_hline(yintercept = c(5,9), linetype = 2)+
      ylab("")+ xlab("")+theme_bw() + facet_wrap(vars(Parametro))


Temp = ggplot(d.t1[d.t1$Parametro==lista3[2],],
             aes(x = Parametro,
                 y = Resultado4))+
  geom_boxplot()+
  geom_hline(yintercept = 40, linetype = 2)+
  ylab("")+ xlab("")+theme_bw() + facet_wrap(vars(Parametro))


sed = ggplot(d.t1[d.t1$Parametro==lista3[3],],
              aes(x = Parametro,
                  y = Resultado4))+
  geom_boxplot()+
  geom_hline(yintercept = 1, linetype = 2)+
  ylab("")+ xlab("")+theme_bw() + facet_wrap(vars(Parametro))

Dqo = ggplot(d.t1[d.t1$Parametro==lista3[4],],
       aes(x = Parametro,
           y = Resultado4))+
  geom_boxplot()+
  geom_hline(yintercept = 120, linetype = 2)+
  ylab("")+ xlab("")+theme_bw() + facet_wrap(vars(Parametro))

Oleo = ggplot(d.t1[d.t1$Parametro==lista3[5],],
       aes(x = Parametro,
           y = Resultado4))+
  geom_boxplot()+
  geom_hline(yintercept = 100, linetype = 2)+
  ylab("")+ xlab("")+theme_bw() + facet_wrap(vars(Parametro))


sulf = ggplot(d.t1[d.t1$Parametro==lista3[6],],
       aes(x = Parametro,
           y = Resultado4))+
  geom_boxplot()+
  geom_hline(yintercept = 1, linetype = 2)+
  ylab("")+ xlab("")+theme_bw() + facet_wrap(vars(Parametro))
fix(d.t1)
ecoli = ggplot(d.t1[d.t1$Parametro == lista3[7], ],
               aes(x = Parametro,
                   y = Resultado4)) +
        geom_boxplot() +
        geom_hline(yintercept = 5000, linetype = 2) +
        ylab("") + xlab("") + theme_bw() + facet_wrap(vars(Parametro))
sst = ggplot(d.t1[d.t1$Parametro == lista3[8], ],
             aes(x = Parametro,
                 y = Resultado4)) +
             geom_boxplot() +
             geom_hline(yintercept = 150, linetype = 2) +
             ylab("") + xlab("") + theme_bw() + facet_wrap(vars(Parametro))
COEMA = plot_grid(ph,Temp,sed,ecoli,
          Dqo,Oleo,sulf,sst,nrow = 2)

ggsave2("Graph/COEMA.png",COEMA,width = 11,height = 4,
        units = "in",limitsize = F,dpi = 2000)

# Nutriente  --------------------------------------------------------------
options(scipen = 100, digits = 4)
lista2
lista_Nut = c("Amônia(como N) - mg/L",
              "Nitrato (como N) - mg/L",
              "Nitrito (como N) - mg/L",
              "Fósfoto Total - mg/L")

c=d = data.frame()
for (j in lista2) {
  for (i in lista_Nut) {#i;j}}
c = data.total[data.total$Coleta== j &
             data.total$Parametro==i,]
d = rbind(c,d)
  }
}
View(d)
d$Parametro = as.factor(as.character(d$Parametro))
levels(d$Parametro) = c(
  "Amônia",  
  "Fósforo Total",   
  "Nitrato",
  "Nitrito"
)


lista_Nut2 = c(
  "Amônia",  
  "Fósforo Total",   
  "Nitrato",
  "Nitrito"
)

N_Amonia = ggplot(d[d$Parametro == "Amônia",], aes(x = Coleta,y = Resultado4))+
  geom_boxplot()+
  geom_hline(yintercept = 20, linetype = 2)+
  facet_grid(Parametro~Amostragrem,scales = "free")+
  theme_bw()+
  stat_summary(fun=mean, geom="point", shape= 8, size=2)+
  ylab("mg/L")+xlab("");N_Amonia

N_P = ggplot(d[d$Parametro == "Fósforo Total",], aes(x = Coleta,y = Resultado4))+
  geom_boxplot()+
  geom_hline(yintercept = 8, linetype = 2,color = 2)+
  facet_grid(Parametro~Amostragrem,scales = "free")+
  theme_bw()+
  stat_summary(fun=mean, geom="point", shape= 8, size=2)+
  ylab("")+xlab("");N_P


N_nitrato = ggplot(d[d$Parametro == "Nitrato",], aes(x = Coleta,y = Resultado4))+
  geom_boxplot()+
  geom_hline(yintercept = 10, linetype = 2)+
  facet_grid(Parametro~Amostragrem,scales = "free")+
  theme_bw()+
  stat_summary(fun=mean, geom="point", shape= 8, size=2)+
  ylab("mg/L")+xlab("");N_nitrato

N_nitrito = ggplot(d[d$Parametro == "Nitrito",], aes(x = Coleta,y = Resultado4))+
  geom_boxplot()+
  geom_hline(yintercept = 1, linetype = 2)+
  facet_grid(Parametro~Amostragrem,scales = "free")+
  theme_bw()+
  stat_summary(fun=mean, geom="point", shape= 8, size=2)+
  ylab("")+xlab("");N_nitrito


N_1 = plot_grid(N_nitrato,N_nitrito,N_P,nrow = 1)
N2 = plot_grid(N_Amonia,N_1,nrow = 2)

ggsave2(filename = "Graph/Nutriente2.png",
        device = "png",
        plot = N2,
        width = 7, 
        height = 5,
        units = "in",
        limitsize = F,
        dpi = 3000)


for (i in 1:4) {
  cat("\n","\n","Analisando = ",lista_Nut2[i],"\n")
  
  a= pairwise.wilcox.test(d$Resultado4[d$Parametro==lista_Nut2[i]],
                          d$Coleta[d$Parametro==lista_Nut2[i]],
                          p.adjust.method ="bonferroni",paired = F,
                          alternative = "t" )
  print(a$p.value)
  cat("\n","\n","####################################################","\n")
}

So = tapply(d$Resultado4[d$Parametro==lista_Nut2[1]],
       d$Coleta[d$Parametro==lista_Nut2[1]],
       median)[2] 
S = tapply(d$Resultado4[d$Parametro==lista_Nut2[1]],
                       d$Coleta[d$Parametro==lista_Nut2[1]],
                       median)[5]


d[d$Parametro==lista_Nut2[1],]


y = (d[d$Parametro == lista_Nut2[1],])

head(y,n = 40)
m = as.factor(month(y$Data_Coleta,label = F))
levels(m) = c("01","02","03","04","05","06",
              "07","08","09","10","11","12")
y$M_A = as.factor(str_c(str_sub(y$Data_Coleta,end = 4),"-",m))

x4 = x2 = x3 = data.frame("Resultado"=NA,
                          "Mes_Ano"=NA,
                          "Coleta" = NA,
                          "Parametro" = NA)
y=y[,c(12:8,2)]

colnames(y) = c("Data","Coleta", 
                "Amostragem","Resultado",
                "Parâmetro", "Tipo de Amostra")

x= y[,c(4,1,2)]
x4 = x2 = x3 = data.frame("Resultado"=NA,
                          "Data"=NA,
                          "Coleta" = NA)
for (i in levels(x$Coleta)) {

    x2 = as.data.frame(
      tapply(y$Resultado[y$Coleta ==i],
             y$Data[y$Coleta ==i],
             function (A){
               mean(A,na.rm = T)
             })
    )

    x2$Data = row.names(x2)
    x2$Coleta = i
    colnames(x2) = c("Resultado", "Data", "Coleta")
    rownames(x2) = 1:NROW(x2)
    x2 = x2[order(x2$Data),]
    x3 = rbind(x3, x2)
    
  }

x3$Data = as.factor(x3$Data)  
x3$Coleta = as.factor(x3$Coleta) 
x3$Coleta = factor(x3$Coleta,levels = c("Anaeróbia",
                                        "Facultativa",
                                        "1ª Maturação",
                                        "2ª Maturação"))
x3 = na.omit(x3)


  for (j in levels(x3$Coleta)) {
    for (i in levels(x3$Data)) {
    x4[i,j] = x3[x3$Data==i&
                x3$Coleta==j,1]
    }
  } 
  


ef.nh3=data.frame(
 "Remoção" =  c(round(((x4[,1]-x4[,2])/x4[,1])*100,2),
    round(((x4[,1]-x4[,3])/x4[,1])*100,2),
    round(((x4[,1]-x4[,4])/x4[,1])*100,2)),
 "local" = rep(c("Facultativa","1ª Maturação","2ª Maturação"),
               c(10,10,10)))
ef.nh3$local = as.factor(ef.nh3$local)
ef.nh3$local = factor(ef.nh3$local, 
                      levels = c("Facultativa",
                                 "1ª Maturação",
                                 "2ª Maturação"))



p = ggplot(ef.nh3, aes(x=local, y = Remoção ))+
  geom_boxplot()+theme_cowplot()+
  geom_hline(yintercept = c(70,80), linetype = 2)+
  stat_summary(fun=mean, geom="point", shape= 8, size=2)+
  ylab("Percentual de remoção")+xlab("")

ggsave2(filename = "Graph/Percentual_Nutriente.png",
        device = "png",
        plot = p,
        width = 6, 
        height = 4,
        units = "in",
        limitsize = F,
        dpi = 3000)
pairwise.wilcox.test(ef.nh3$Remoção,
                     ef.nh3$local,
                     p.adjust.method = "bonferroni",
                     alternative = "t", paired = F)


# OD ----------------------------------------------------------------------

od = data.total[data.total$Parametro =="Oxigênio Dissolvido - mg/L",]
od[sort(od$Coleta),]
m = as.factor(month(od$Data_Coleta,label = F))
levels(m) = c("01","02","03","04","05","06",
              "07","08","09","10","11","12")
od$Data = as.factor(str_c(str_sub(od$Data_Coleta,end = 4),"-",m))


 od_anali = ggplot(od, aes(x = Coleta,y = Resultado4))+
  geom_boxplot()+
  geom_hline(yintercept = 5, linetype = 2)+
  facet_grid(Parametro~Amostragrem,scales = "free")+
  theme_bw()+
  stat_summary(fun=mean, geom="point", shape= 8, size=2)+
  ylab("")+xlab("")

 ggsave2(filename = "Graph/OD_analitico.png",
         device = "png",
         plot = od_anali,
         width = 6, 
         height = 4,
         units = "in",
         limitsize = F,
         dpi = 3000)
 
  
 od_trat = ggplot(od, aes(x = Amostragrem,y = Resultado4))+
   geom_boxplot()+
   geom_hline(yintercept = 5, linetype = 2)+
   facet_grid(Parametro~Amostragrem,scales = "free")+
   theme_bw()+
   stat_summary(fun=mean, geom="point", shape= 8, size=2)+
   ylab("")+xlab("")

 
 pairwise.wilcox.test(od$Resultado4,
                      od$Amostragrem,
                      p.adjust.method = "bonferroni",
                      alternative = "t", paired = F,exact = F)
 

# DBO ---------------------------------------------------------------------

 dbo = data.total[data.total$Parametro =="DBO - mg/L",]
 dbo[sort(dbo$Coleta),]
 dbo$Parametro2 = "Carga de DQO (Kg/Dia)"
 summary(dbo)
 m = as.factor(month(dbo$Data_Coleta,label = F))
 levels(m) = c("01","02","03","04","05","06",
               "07","08","09","10","11","12")
dbo$Data = as.factor(str_c(str_sub(dbo$Data_Coleta,end = 4),"-",m))
 
 
 dbo_anali = ggplot(dbo, aes(x = Coleta,y = (Resultado4*4.049)))+
   geom_boxplot()+
   geom_hline(yintercept = c(2075), linetype = 2)+
   facet_grid(Parametro2~Amostragrem,scales = "free")+
   theme_bw()+
   stat_summary(fun=mean, geom="point", shape= 8, size=2)+
   ylab("")+xlab(""); dbo_anali

 ggsave2(filename = "Graph/Carga_analitico.png",
         device = "png",
         plot = dbo_anali,
         width = 6, 
         height = 4,
         units = "in",
         limitsize = F,
         dpi = 3000)
 

# SST ---------------------------------------------------------------------

 sst = data.total[data.total$Parametro =="Sólidos Suspensos Totais - mg/L",]
 sst[sort(sst$Coleta),]
 #dbo$Parametro2 = "Carga de DQO (Kg/Dia)"
 summary(sst)
 m = as.factor(month(sst$Data_Coleta,label = F))
 levels(m) = c("01","02","03","04","05","06",
               "07","08","09","10","11","12")
 sst$Data = as.factor(str_c(str_sub(sst$Data_Coleta,
                                    end = 4),"-",m))
 
 
 sst_anali = ggplot(sst, aes(x = Coleta,y = (Resultado4)))+
   geom_boxplot()+
   #geom_hline(yintercept = c(2075), linetype = 2)+
   facet_grid(Parametro~Amostragrem,scales = "free")+
   theme_bw()+
   stat_summary(fun=mean, geom="point", shape= 8, size=2)+
   ylab("")+xlab(""); sst_anali
 
 ggsave2(filename = "Graph/sst.png",
         device = "png",
         plot = dbo_anali,
         width = 6, 
         height = 4,
         units = "in",
         limitsize = F,
         dpi = 3000)
 
 pairwise.wilcox.test(sst$Resultado4,
                      sst$Coleta,
                      p.adjust.method = "bonferroni",
                      alternative = "t", paired = F,exact = F)
 
 
 
 length(dbo[dbo$Coleta=="2ª Maturação",])
 levels(dbo[dbo$Coleta=="2ª Maturação",])
summary(dbo[dbo$Coleta=="2ª Maturação",]) 
summary(sst[sst$Coleta=="2ª Maturação",])


Lista_data = c("2021-01",
               "2021-04",
               "2022-01",
               "2022-02")
r = data.frame()
for (i in Lista_data) {
  r[i,"sst1/dbo"] =sst[sst$Coleta == "2ª Maturação" &
        sst$Data ==i, 9][1] /
    dbo[dbo$Coleta == "2ª Maturação" &
          dbo$Data == i,9 ]
  r[i,"sst2/dbo"] =sst[sst$Coleta == "2ª Maturação" &
                        sst$Data ==i, 9][2] /
    dbo[dbo$Coleta == "2ª Maturação" &
          dbo$Data == i,9 ]
}

r$Razão = "Razão SST/DBO"

razao = ggplot(r, aes(x = Razão,y = `sst1/dbo`))+
  geom_boxplot()+
  geom_hline(yintercept = c(1), linetype = 2)+
  facet_grid(Razão~.,scales = "free")+
  theme_bw()+
  stat_summary(fun=mean, geom="point", shape= 8, size=2)+
  ylab("")+xlab(""); razao




# Coagulação  --------------------------------------------------------------

coag = data.total[data.total$Coleta =="2ª Maturação",]

m = as.factor(month(coag$Data_Coleta,label = F))
levels(m) = c("01","02","03","04","05","06",
              "07","08","09","10","11","12")
coag$Data = as.factor(str_c(str_sub(coag$Data_Coleta,
                                   end = 4),"-",m))

coag$Parametro = as.factor(as.character(coag$Parametro)) 

levels(coag$Parametro)
summary(coag$Parametro)
"Temperatura" 
"pH"
"Cor Aparente"
"Cor Verdadeira"
d =coag[coag$Parametro=="Cor Aparente - uH"#,] 
                 |coag$Parametro=="Cor Verdadeira -uH" ,]

a =  ggplot( d, aes(x = Parametro,y = Resultado4))+
   geom_boxplot()+
  #geom_hline(yintercept = c(1), linetype = 2)+
  facet_grid(Coleta~Parametro,scales = "free")+
  theme_bw()+
  stat_summary(fun=mean, geom="point", shape= 8, size=2)+
  ylab("")+xlab("");a

b = ggplot(d,aes(x = Parametro,y = Resultado4))+
  geom_boxplot()+
  #geom_hline(yintercept = c(1), linetype = 2)+
  facet_grid(Coleta~Parametro,scales = "free")+
  theme_bw()+
  stat_summary(fun=mean, geom="point", shape= 8, size=2)+
  ylab("")+xlab("");b
c = plot_grid(b,a);c

ggsave2(filename = "Graph/Cor2.png",
        device = "png",
        plot = a,
        width = 5, 
        height = 4,
        units = "in",
        limitsize = F,
        dpi = 3000)
