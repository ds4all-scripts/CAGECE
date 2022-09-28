# libraries  --------------------------------------------------------------
library(dygraphs)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(stringr)
library(cowplot)
library(ggplot2)
library(forecast)

setwd("D:/Documents/Projects/CAGECE")

# datasets ----------------------------------------------------------------


UTR27 = readxl::read_xlsx("Datasets/UTR27.xlsx")
UTR28 = readxl::read_excel("Datasets/UTR28.xlsx")
UTR43 = readxl::read_xlsx("Datasets/UTR43.xlsx")

tail(UTR27)

#Time series ------------------------------------------------------------

