library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gganimate)
library(gifski)
library(png)

apps <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-24/apps_googleplaystore.csv")

apps2<-apps%>% 
  filter(clasificacion_contenido!= "Sin clasificación")%>% 
  filter(clasificacion_contenido!= "Adultos unicamente")

library(gganimate)
library(gifski)
library(png)


b<-ggplot(apps2,aes(x=apps2$clasificacion_contenido, y=apps2$calificacion, fill=apps2$tipo))+geom_boxplot()+labs(title="Aplicaciones de Google Play Store",
      x="Clasificación de Contenido", y = "Calificaciónn")+scale_fill_manual(values= c("#006633","red"))+theme_minimal()


c<-b+transition_states(
  apps2$clasificacion_contenido,
  transition_length = 2,
  state_length = 1) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
c

anim_save("AppsStore.gif",c)