library(tidyverse)
library(dplyr)
library(ggplot2)

datos_uip <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv")

summary(datos_uip)

datos_uip_sinNA <- datos_uip[!is.na(datos_uip$camara),]
sapply(datos_uip_sinNA, function(x) sum(is.na(x)))

sapply(datos_uip, function(x) sum(is.na(x)))
datos_uip_sinNA <- datos_uip[!is.na(datos_uip$porcentaje_mujeres),]
sapply(datos_uip_sinNA, function(x) sum(is.na(x)))

datos_uip_sinNA<-na.omit(datos_uip_sinNA)


# Change box plot colors by groups

attach(datos_uip_sinNA)

a<-ggplot(datos_uip_sinNA, aes(x=camara, y=porcentaje_mujeres, 
                        fill=cuota_genero)) +geom_boxplot() +labs(title="Mujeres en Pol�tica",x="C�mara", y = "Porcentaje de mujeres en esa c�mara")+scale_fill_manual(values= c("#4271AE","#56B4E9"))+theme_minimal()

a

devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(gifski)
library(png)


b<-a+transition_states(
  camara,
  transition_length = 2,
  state_length = 1) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
b
anim_save("Mujeres_politica.gif",b)

