# Path, libraries and datasets --------------------------------------------

setwd("D:/Documents/Projects/CAGECE")

library(forecast)
library(ggplot2)
library(cowplot)
library(xts)
library(readxl)

Aldeota <- read_excel("Datasets/Aldeota-Mucuripe/Aldeota_07.xls", 
                      col_types = c("text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric"))
A = as.data.frame(Aldeota)


# PIT-007-S01-000	Medidor Pressão Saída Reservatório Mucuripe 
# PIT-007-B03-000	Medidor Pressão da CMB03
# PIT-007-B01-000	Medidor Pressão da CMB01
# PIT-007-B02-000	Medidor Pressão da CMB02
# PIT-007-REL-000	Medidor Pressão Elevado Aldeota
# FIT-007-S01-VLM	Medidor vazão EP021 Saída Reservatório Mucuripe Mensal 
# FIT-007-S01-VLD	Medidor vazão EP021 Saída Reservatório Mucuripe Diário
# FIT-007-S01-000	Medidor vazão EP021 Saída Reservatório Mucuripe


# Medidor de Pressão Sáida Mucuripe -----------------------------------------


P.Aldeota = (A$`PIT-007-S01-000`)
bp = boxplot(P.Aldeota,
             ylim = c(0,5),ylab = "kgf/cm²",
             xlab = "PIT-007-S01-000",
             main ="Medidor Pressão Saída Reservatório Mucuripe ")

#Pressão Max.
round(bp$stats[5]*10.33,1)
round(bp$stats[1]*10.33,1)

#47.7 mca (sistema de bombemento)

# Medidor de Vazão Sáida Mucuripe -------------------------------------------------------------------------


V.Aldeota = (A$`FIT-007-S01-000`)
bv = boxplot(V.Aldeota,
            ylab = "m³/h",
             xlab = "FIT-007-S01-000",
             main ="Medidor vazão EP021 Saída Reservatório Mucuripe")

#Vazão min.
round(bv$stats[1]*(1.02^20)*0.278,1)

#Vazão Max.
round(bv$stats[5]*(1.02^20),1)

#677 L/s (Final de plano de 20 anos)