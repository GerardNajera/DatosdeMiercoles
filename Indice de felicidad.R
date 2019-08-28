library(tidyverse)
library(dplyr)
library(ggplot2)
library(visdat)
library(corrplot)
library(viridis)
library(Hmisc)
library(ggcorrplot)

felicidad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-07/felicidad.csv")
summary(felicidad)
names(felicidad)

#Visualizar NA'S
vis_miss(felicidad, sort_miss=TRUE)

cor_felicidad<-cor(felicidad[,3:14],use="complete.obs")
cor_felicidad

win.graph()
ggcorrplot(cor_felicidad, type = "lower",  colors = c("#6D9EC1", "white", "#E46726"),
           lab=TRUE,title = "Matriz de correlación con datos del reporte de felicidad mundial",
           ggtheme =ggplot2::theme_minimal )


