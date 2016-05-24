#rm(list=ls(all=TRUE))
setwd("C:/Users/Patrycja/Documents/R/Faza 2")
ICD<-read.table(file = "C:/Users/Patrycja/Documents/R/Faza 2/dane_podstawowe_ICD.csv", 
           sep=";", dec=",", header=TRUE)

str(ICD)
ICD$grupa_wiek <- as.character(ICD$grupa_wiek)
ICD$grupa_wiek[ICD$grupa_wiek == "<0-44>"] <- "0-44"
ICD$grupa_wiek[ICD$grupa_wiek == "<45-54>"] <- "45-54"
ICD$grupa_wiek[ICD$grupa_wiek == "<55-64>"] <- "55-64"
ICD$grupa_wiek[ICD$grupa_wiek == "<65-74>"] <- "65-74"
ICD$grupa_wiek[ICD$grupa_wiek == "<75-84>"] <- "75-84"
ICD$grupa_wiek[ICD$grupa_wiek == "85+"] <- "85+"

ICD$TERYT4<-sprintf("%04d", ICD$TERYT4)

library(dplyr)

ICD %>% 
  
  group_by(ICD10, PLEC, grupa_wiek, TERYT4, woj, pow, rok) %>%
  
  summarise(Stage1 = sum(Stage1), 
            
            Stage2 = sum(Stage2), 
            
            Stage3 = sum(Stage3), 
            
            Stage4 = sum(Stage4)) -> suma

suma$suma<-rowSums(suma[,c(8:11)])

str(ICD)