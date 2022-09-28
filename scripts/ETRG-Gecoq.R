#Análise dados Geocoq

A = read.csv("D:/Google Drive/Dados Gecoq/2019 - UN-BAC.csv")
B = read.csv("D:/Google Drive/Dados Gecoq/2020 - UN-BAC.csv")
C = read.csv("D:/Google Drive/Dados Gecoq/2021 - UN-BAC.csv")
D = rbind(A,B,C)

unique(D$Natureza.da.amostra)
# "MANANCIAL SUPERFICIAL" "ENTRADA DE TRATAMENTO" "MANANCIAL SUBTERRÂNEO"

unique(D$Sistema) # Sistema analisado similar à Localidade 

unique(D$Localidade) # Sistema analisado similar ao Sistema

unique(D$Objetivos.de.coleta) # Não definido ! Verificar no laboratório

unique(D$Ponto.de.coleta) # Não definido ! Verificar no laboratório

unique(D$Ponto.de.amostragem) # Não definido! Verificar a diferença entre Ponto de Coleta e de amostragem

unique(D$) 

Super = D[D$Objetivos.de.coleta == "MENSAL (MANANCIAL SUPERFICIAL)"|
          D$Objetivos.de.coleta == "MENSAL (MANANCIAL SUPERFICIAL), SEMESTRAL (MANANCIAL SUPERFICIAL)" |
          D$Objetivos.de.coleta =="SEMESTRAL (REDE DE DISTRIBUIÇÃO), MENSAL (MANANCIAL SUPERFICIAL), SEMESTRAL (MANANCIAL SUPERFICIAL)",]

Tratada = D[D$Objetivos.de.coleta == "DIÁRIO (REDE DE DISTRIBUIÇÃO)"|
            D$Objetivos.de.coleta == "MENSAL (SAÍDA DE TRATAMENTO)",] 

sub =  D[D$Objetivos.de.coleta ==  "MENSAL (MANANCIAL SUBTERRÂNEO)" |
         D$Objetivos.de.coleta == "MENSAL (MANANCIAL SUBTERRÂNEO), SEMESTRAL (MANANCIAL SUBTERRÂNEO)"|
         D$Objetivos.de.coleta == "MENSAL (SAÍDA DE TRATAMENTO), SEMESTRAL (MANANCIAL SUBTERRÂNEO)" ,]

Misto = D[D$Objetivos.de.coleta == "MENSAL (MANANCIAL SUBTERRÂNEO), SEMESTRAL (MANANCIAL SUPERFICIAL)"|
          D$Objetivos.de.coleta == "MENSAL (MANANCIAL SUPERFICIAL), SEMESTRAL (MANANCIAL SUBTERRÂNEO)" ,]  

par(mar = c(16,4,2,1))
barplot(sort(table(Super$Sistema),decreasing = T), las = 2)

(table(Super$Sistema)))
