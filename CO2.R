library(tidyverse)
library(readr)
library(viridis)
library(ggthemes)
library(devtools)
library(gganimate)
library(gifski)

co2_ingreso <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-21/co2_ingreso.csv")

summary(co2_ingreso$anio)

p <- ggplot(co2_ingreso,aes(anio, emision_co2, group =grupo , color = factor(grupo))) +
  geom_line(linetype = "solid",size=0.8,alpha=1)+
  scale_colour_hue()+scale_x_continuous(breaks = seq(1960, 2014, 5), 
                                        limits=c(1960, 2014))+
  ggtitle("Emisi�n de CO2 por grupo de pa�ses seg�n su ingreso",
          subtitle = "A�os: 1960-2014") +
  labs(x = "A�o", y = "Emisi�n de CO2 (toneladas m�tricas per c�pita)") +
  theme(legend.position = "top")+theme_bw()

p+ geom_point(aes(group = seq_along(anio))) +
  transition_reveal(anio)

anim_save( "co2.gif")