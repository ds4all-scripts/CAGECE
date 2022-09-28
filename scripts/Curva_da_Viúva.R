# libraries  --------------------------------------------------------------
library(dygraphs)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(stringr)
library(cowplot)
library(ggplot2)
library(forecast)
library(qcc)


# Datasets ----------------------------------------------------------------

UTR64 = readxl::read_xlsx("Datasets/UTR64.xlsx")
UTR64


  summary(UTR64$`Pressão Saída (mca)`
        )
boxplot(UTR64$`Pressão Entrada (mca)`)
plot(UTR64$`Pressão Entrada (mca)`, type = "l",xaxt = "n")

axis(1, 
     1:length(UTR64$`Vazão (L/s)`),
     str_c(UTR64$Dia," - ",UTR64$Hora, " h"),
     las = 2)
ggplot(UTR64, aes(x = as.factor(str_c(Dia," - ",Hora, " h")),
                  y=`Vazão (L/s)`))+geom_line()
par(mfrow = c(1,3),bty = "n")
a = boxplot(UTR64$`Pressão Entrada (mca)`,
            xlab = "Pressão Entrada",ylab = "mca")
mn.t <- mean(UTR64$`Pressão Entrada (mca)`)
sd.t <- sd(UTR64$`Pressão Entrada (mca)`)
xi <- .75
points(xi, mn.t, col = "black", pch = 8)

c = boxplot(UTR64$`Pressão Saída (mca)`,
            xlab = "Pressão Saída",
            ylab = "mca")
mn.t <- mean(UTR64$`Pressão Saída (mca)`)
sd.t <- sd(UTR64$`Pressão Saída (mca)`)
xi <- .75
points(xi, mn.t, col = "black", pch = 8)

b = boxplot(UTR64$`Vazão (L/s)`,xlab = "Vazão",ylab = "L/s")
mn.t <- mean(UTR64$`Vazão (L/s)`)
sd.t <- sd(UTR64$`Vazão (L/s)`)
xi <- .75
points(xi, mn.t, col = "black", pch = 8)

