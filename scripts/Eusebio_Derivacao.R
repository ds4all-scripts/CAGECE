
# ADUTORA PICI-FLORESTA ---------------------------------------------------

setwd("D:/Documents/Projects/CAGECE")
library(readxl)
UTR27_A_B <- read_excel("Datasets/UTR_Ref.Messejana.xlsx", 
                        col_types = c("text", "numeric", 
                                      "numeric","numeric"))
View(UTR27_A_B)
summary(UTR27_A_B)

# Maior Vazão -------------------------------------------------------------


(a = UTR27_A_B[which(UTR27_A_B$MESSEJANA_27A>1 &
                          UTR27_A_B$PEDRAS_27A>1 &
                           UTR27_A_B$MESSEJANA_27B>1 ),])


png(filename = "Graph/Máx_vazao_Eusebio.png",
    width = 8,
    height = 6,
    units = "in",res = 400)
par(mfrow = c(2,3))
b = boxplot(a$MESSEJANA_27A,
            ylab = "L/s", 
            xlab = "Vazão na UTR 27A (Messejana)",
            main = "FIT-27A-S01",
            sub = "(a)")

b$stats

c = boxplot(a$PEDRAS_27A,
            ylab = "L/s", 
            xlab = "Vazão na UTR 27A (Pedras)", 
            main = "FIT-27A-S02",
            sub = "(b)")
c$stats


d = boxplot(a$MESSEJANA_27B,
            ylab = "L/s", 
            xlab = "Vazão na UTR 27B (MESSEJANA)",
            main = "FIT-27B-S01",sub = "(c)")

d$stats



#Def. dos pontos de Max.vazão

#27A-MESSEJANA

s = b$stats[5]*1.1
i = b$stats[5]*.9
MAX = as.data.frame(a[a$MESSEJANA_27A<s &
          a$MESSEJANA_27A>i,])

hist(MAX[,2], 
     xlab = "Maiores vazões em L/s na UTR-27A (Messejana)",
     ylab = "Frequência",
     main = "FIT-27A-S01",freq = F)
abline(v = mean(MAX[,2]), 
       col = 2,lty = 2,lwd = 2)


#27A-PEDRAS
s = c$stats[5]*1.1 
i = c$stats[5]*.9
MAX = as.data.frame(a[a$PEDRAS_27A<s &
                        a$PEDRAS_27A>i,])

hist(MAX[,3], 
     xlab = "Maiores vazões em L/s na UTR-27A (Pedras)",
     ylab = "Frequência",
     main = "FIT-27A-S02",
     freq = F)
abline(v = mean(MAX[,3]), 
       col = 2,lty = 2,lwd = 2)

#27B-MESSAJANA
s = d$stats[5]*1.1 
i = d$stats[5]*.9
MAX = as.data.frame(a[a$MESSEJANA_27B<s &
                      a$MESSEJANA_27B>i,])

hist(MAX[,4], 
     xlab = "Maiores vazões em L/s na UTR-27B (Messejana)",
     ylab = "Frequência",
     main = "FIT-27B-S01",freq = F)
abline(v = mean(MAX[,4]), 
       col = 2,lty = 2,lwd = 2)
dev.off()

