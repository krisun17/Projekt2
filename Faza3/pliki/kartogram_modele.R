#Wybieram .xls lub .RData:

#setwd("C:/Users/Patrycja/Documents/R/Faza 3")

#require(XLConnect)
#wybieram plik .xls
#wkb <- loadWorkbook(file.choose())
#dataLoad <- readWorksheet(wkb,length(getSheets(wkb)),startCol=1)
#dat<-dataLoad
#dat$TERYT<-substring(dat$TERYT, 1, 4)

#load("dat.Rdata")

#Rysujê kartogram
load(file="tract.RData")
str(tract)
mean(tract$lat)
  
tract$spr<-0
tract$spr[tract$lat>=422923.6]<-1
tract$spr[tract$spr=="1"]<-"Model 1"
tract$spr[tract$spr=="0"]<-"Model 2"
#ksztalt<-tract[order(tract$spr),]
ksztalt<-tract

#library(dplyr)

#ksztalt<-left_join(x = tract, y = dat, by = "TERYT")
str(ksztalt)


library(ggplot2)
library(ggmap)
library(scales)


#je¿eli dane w procentach, to zamieniam na:
#scale_fill_distiller(palette = "Oranges", labels = percent, breaks = pretty_breaks() , values = c(1,0))
modelcolours = c("#FF0033", "#000000")
p1 <- ggplot(data = ksztalt, aes(x = long, y = lat, group = group), color = "black", size = 0.05) +
  geom_polygon(aes(fill=spr), color = "grey50")+
  scale_fill_manual(name = "Numer modelu", 
                    values = modelcolours) +
  guides(fill = guide_legend(reverse = FALSE)) + #tutaj mo¿na zmieniæ na TRUE
  theme_nothing(legend = TRUE) +
  labs(title = "Tytul",
       fill = "") +
  coord_equal()
p1

