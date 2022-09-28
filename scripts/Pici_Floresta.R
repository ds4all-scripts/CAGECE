
# ADUTORA PICI-FLORESTA ---------------------------------------------------

setwd("D:/Documents/Projects/CAGECE")
library(readxl)
Teste <- read_excel("Datasets/UTR12.xlsx")
View(Teste)
summary(Teste)

# Maior Vazão -------------------------------------------------------------

#550 
#Retirar 0

a = Teste[which(Teste$Q_700>30),]

par(mfrow = c(2,2))
b = boxplot(a$P_550,
            ylab = "mca", 
            xlab = "Pressão linha 550",
            main = "PIT-012-S01",
            sub = "(a)")

b$stats

c = boxplot(a$Q_550,
            ylab = "L/s", 
            xlab = "Vazão linha 550", 
            main = "FIT-012-S01",
            sub = "(b)")
c$stats


d = boxplot(a$P_700,
            ylab = "mca", 
            xlab = "Pressão linha 700",
            main = "PIT-012-S02",sub = "(c)")

d$stats

e = boxplot(a$Q_700,
            ylab = "L/s", 
            xlab = "Vazão linha 700", 
            main = "FIT-012-S02",
            sub = "(d)")
e$stats



#Def. dos pontos de Max.Pressão
#550
summary(a)
s = c$stats[5]*1.05 
i = c$stats[5]*.95
View(data[data$P_550<s &
            data$P_550>i,])
par(mfrow = c(2,2))
hist(data[data$P_550<s &
            data$P_550>i,2], 
     xlab = "Maiores Pressões em mca na Linha DN 550",
     ylab = "Frequência",
     main = "PIT-012-S01")
abline(v = mean(data[data$P_550<s &
                       data$P_550>i,2]), 
                       col = 2,lty = 2,lwd = 2)

hist(data[data$P_550<s &
            data$P_550>i,3], 
     xlab = "Vazões em L/s na Linha DN 550",
     ylab = "Frequência",
     main = "FIT-012-S01")
abline(v = mean(data[data$P_550<s &
                       data$P_550>i,3]), 
       col = 2,lty = 2,lwd = 2)


#DN 700
s = d$stats[5]*1.05;s 
i = d$stats[5]*.95;i
hist(data[data$P_700<s &
            data$P_700>i,5], 
     xlab = "Maiores Pressões em mca na Linha DN 700",
     ylab = "Frequência",
     main = "PIT-012-S02")
abline(v = mean(data[data$P_700<s &
                       data$P_700>i,5]), 
       col = 2,lty = 2,lwd = 2)

hist(data[data$P_700<s &
            data$P_700>i,4], 
     xlab = "Vazões em L/s na Linha DN 700",
     ylab = "Frequência",
     main = "FIT-012-S02")
abline(v = mean(data[data$P_700<s &
                       data$P_700>i,4]), 
       col = 2,lty = 2,lwd = 2)

#Def. dos pontos de Max.Vazão
#550
s = c$stats[5]*1.05;s 
i = c$stats[5]*.95;i
data = as.data.frame(a)
summary(a)
View(data[data$Q_550<s &
            data$Q_550>i,])
par(mfrow = c(2,2))
hist(data[data$Q_550<s &
            data$Q_550>i,2], 
     xlab = "Pressões em mca na Linha DN 550",
     ylab = "Frequência",
     main = "PIT-012-S01")
abline(v = median(data[data$Q_550<s &
                       data$Q_550>i,2]), 
       col = 2,lty = 2,lwd = 2)

hist(data[data$Q_550<s &
            data$Q_550>i,3], 
     xlab = "Maiores Vazões em L/s na Linha DN 550",
     ylab = "Frequência",
     main = "FIT-012-S01")
abline(v = median(data[data$Q_550<s &
                       data$Q_550>i,3]), 
       col = 2,lty = 2,lwd = 2)


#DN 700
s = e$stats[5]*1.05;s 
i = e$stats[5]*.95;i
hist(data[data$Q_700<s &
            data$Q_700>i,5], 
     xlab = "Pressões em mca na Linha DN 700",
     ylab = "Frequência",
     main = "PIT-012-S02")
abline(v = median(data[data$Q_700<s &
                       data$Q_700>i,5]), 
       col = 2,lty = 2,lwd = 2)

hist(data[data$Q_700<s &
            data$Q_700>i,4], 
     xlab = "Maiores Vazões em L/s na Linha DN 700",
     ylab = "Frequência",
     main = "FIT-012-S02")
abline(v = mean(data[data$Q_700<s &
                       data$Q_700>i,4]), 
       col = 2,lty = 2,lwd = 2)


# Derivações --------------------------------------------------------------

setwd("D:/Documents/Projects/CAGECE")
library(readxl)
Teste <- read_excel("Datasets/DERV_PICI_Foresta.xls",
                    sheet = "UTR58" )
Teste2 <- read_excel("Datasets/DERV_PICI_Foresta.xls",
                    sheet = "UTR17" )
Teste3 <- read_excel("Datasets/DERV_PICI_Foresta.xls",
                    sheet = "UTR47" )
View(Teste)
summary(Teste)

# Maior Vazão -------------------------------------------------------------

a  = as.data.frame(Teste[which(Teste$Q>5),])
a1 = as.data.frame(Teste2[which(Teste2$Q>5),])
a2 = as.data.frame(Teste3[which(Teste3$Q>5),])

png(filename = "Graph/derivacoes.png",width = 8,height = 6,
    units ="in",res = 400 )
par(mfrow = c(3,2))

#UTR-58 - Valor de referência
b = boxplot(a$Q,
            ylab = "L/s", 
            xlab = "Vazão na UTR 58 - Ref. Pici",
            main = "FIT-058-S01",
            sub = "(a)")

b$stats

#Def. do intervalo de referência UTR-58

s = b$stats[5]*1.1 
i = b$stats[5]*.9

hist(a[a$Q<s &
         a$Q>i,2], 
     xlab = "Maiores vazões em L/s na UTR 58 - Ref. Pici",
     ylab = "Frequência",
     main = "FIT-058-S01",sub = "(b)")
abline(v = median(data[data$Q<s &
                         data$Q>i,2]), 
       col = 2,lty = 2,lwd = 2)


#UTR-47 - Valor de referência
b2 = boxplot(a2$Q,
             ylab = "L/s", 
             xlab = "Vazão na UTR 47 - Papi JR",
             main = "FIT-047-S01",
             sub = "(c)")

b2$stats

#Def. do intervalo de referência UTR-47
s = b2$stats[5]*1.1 
i = b2$stats[5]*.9

hist(a2[a2$Q<s &
          a2$Q>i,2], 
     xlab = "Maiores vazões em L/s na UTR 47 - Papi JR",
     ylab = "Frequência",
     main = "FIT-047-S01",sub = "(d)")
abline(v = median(a2[a2$Q<s &
                       a2$Q>i,2]), 
       col = 2,lty = 2,lwd = 2)

#UTR-17 - Valor de referência
b1 = boxplot(a1$Q,
            ylab = "L/s", 
            xlab = "Vazão na UTR 17 - Amadeu Furtado",
            main = "FIT-017-S01",
            sub = "(e)")

b1$stats

#Def. do intervalo de referência UTR-17 
s = b1$stats[5]*1.1 
i = b1$stats[5]*.9

hist(a1[a1$Q<s &
          a1$Q>i,2], 
     xlab = "Maiores vazões em L/s na UTR 17 - Amadeu Furtado",
     ylab = "Frequência",
     main = "FIT-017-S01",sub = "(f)")
abline(v = median(a1[a1$Q<s &
                       a1$Q>i,2]), 
       col = 2,lty = 2,lwd = 2)
dev.off()
