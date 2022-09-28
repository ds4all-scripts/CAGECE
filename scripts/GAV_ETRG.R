
# Work directory, Libraries and Dataset ----------------------------------

# Work directory
setwd("D:/Documents/Projects/CAGECE")

# Libraries 

library(readxl)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(reshape)

# Dataset 
 ETRG <- read_excel("Datasets/ETRG.xlsx", 
                     sheet = "DATASET", 
                    col_types = c("text",
                                  "date", 
                                  "text", 
                                  "text",
                                  "text",
                                  "skip",
                                  "skip",
                                  "skip"))


# Data visualization --------------------------------------------------------

summary(ETRG)

ETRG$Parâmetro = as.factor(ETRG$Parâmetro)

table(ETRG$Parâmetro)

(ETRG[ETRG$Parâmetro=="Al",])

A = ETRG[ETRG$Parâmetro=="Al",]
B = ETRG[ETRG$Parâmetro=="T",]

summary(ETRG$`Data da coleta`)

d = (unique(ETRG$fator))
d1[1] =  "2018-06-12 08:00:00 UTC"

p = c( "Al", "DQO", "MF", "pH", "SS", "SST", "T")

ETRG$fator = as.factor(ETRG$`Data da coleta`)
# Dataset -----------------------------------------------------------------

d1= data.frame()
for (i in 1:length(d)) {
  for (j in p) {
    d1[i,"AMOSTRA"] = ETRG[ETRG$Parâmetro==j &
                           ETRG$fator== d[i],1]
    d1[i,"DATA"] = ETRG[ETRG$Parâmetro==j &
                        ETRG$fator==d[i],2]
    d1[i,j] = ETRG[ETRG$Parâmetro==j &
                   ETRG$fator==d[i],4]
    d1[i,"OK"] = ETRG[ETRG$Parâmetro==j &
                      ETRG$fator==d[i],5]
    
  }
}

# 1 -> "AMOSTRA"
# 2 -> "DATA"
# 4 -> j
# 5 -> "OK"


