#rm(list=ls(all=TRUE))
setwd("C:/Users/Patrycja/Documents/R/Faza 2")
library(dplyr)

load(file="suma.RData")

#Pobieramy dane z GUS dotycz¹ce ludnoœci Polski

load(file="ludnosc_woj_og.RData")


suma %>%
  group_by( woj, PLEC ) %>%
  summarize( suma=sum(suma)) -> suma1

l_j<-left_join(x = suma1, y = ludnosc_woj_og, by = "woj")

#Normalizujemy - mno¿ymy przez 3, bo mamy sumê dla 3 lat: 2012, 2011, 2010

l_j$norm<-l_j$suma/(l_j$ogolem*3)

lj<-l_j[,c(1,2,7,8)]
lj<-unique(lj)

lj<-lj[order(lj$PLEC,lj$norm),]

rownames(lj)<-1:32

max(lj$norm)
str(lj)

library(ggvis)

lj %>% ggvis(~woj_nazwa, ~norm, fill = ~PLEC) %>%
  layer_points() %>%
  add_axis("x", title = "Nazwa województwa",title_offset = 100, subdivide = 0, properties = axis_props(labels = list(angle = 315, align = "right"))) %>%
  add_axis("y", title = "Odsetek", title_offset = 50, subdivide = 0) %>%
  add_legend("fill", title = "P³eæ") %>% 
  add_axis("x", orient = "top", ticks = 0, title = "Odsetek zachorowañ wzglêdem województw z podzia³em na p³eæ",
           properties = axis_props(
             axis = list(stroke = "white"),
             labels = list(fontSize = 0)))
