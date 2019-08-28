library(tidyverse)
library(readr)

capitulos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/capitulos_rladies.csv")
eventos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/eventos_rladies.csv")


capitulos2 <- capitulos_rladies %>% 
  group_by(ciudad)%>% 
  summarise(conteo=n(), miembros_promedio = mean(miembros, na.rm = TRUE))%>% 
  filter(miembros_promedio>650)

capitulos_ordenado<-capitulos2%>% 
  arrange(miembros_promedio)%>%
  select(ciudad,miembros_promedio)
  
  
  capitulos_ordenado%>% 
    arrange(miembros_promedio) %>%  
    mutate(ciudad=factor(ciudad,ciudad)) %>%
    ggplot( aes(x=ciudad, y=miembros_promedio)) +
    geom_segment( aes(xend=ciudad, yend=0)) +
    geom_point( size=5, color="darkblue",alpha=0.6) +
    coord_flip() +
    xlab("Ciudad") +
    ylab("Miembros")+
    labs(title="Top 20 de capitulos de Rladies con más miembros") +  
      theme_bw()
  
  ggsave("rladies.png")
  