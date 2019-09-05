library(tidyverse)
library(readr)
library(viridis)
library(ggmap)
library(maps)
library(ggthemes)
library(devtools)
library(gganimate)
library(gifski)

# install.packages('devtools')
devtools::install_github('thomasp85/gganimate')

# install_packages("readr")
felicidad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-07/felicidad.csv")

summary(felicidad$anio)
summary(felicidad$escalera_vida)

hist(felicidad$escalera_vida,main = "Puntaje de felicidad/Escalera de la vida 2005-2018",xlab = "Puntaje felicidad/Escalera de vida",
     col =inferno(12),xlim = c(2,9), breaks = 19.5)


world <- map_data("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                       "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", 
                                       "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
                                       "Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica,Dominica"))
world <- fortify(world)

felicidad18 <- felicidad %>% select(pais, escalera_vida, anio) %>% filter(anio == 2018)

felicidad18$pais<- chartr('áéíóú','aeioun',felicidad18$pais)
felicidad18$pais<- chartr('Brasil','Brazil',felicidad18$pais)

win.graph()
ggplot() + 
  geom_map(data=world, map=world,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") + 
  geom_map(data=felicidad18, map=world,
           aes(fill=escalera_vida, map_id=pais),
           colour="black")+
  scale_fill_viridis_c(option = "C")+
  labs(title = "Puntaje de felicidad/Escalera de la vida de Latinoamérica 2018")+theme_void()


###################################################################################################3

