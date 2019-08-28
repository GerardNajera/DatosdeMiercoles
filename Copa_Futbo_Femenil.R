library(tidyverse)
library(readr)
library(ggridges)
library(forcats)
library(data.table)
library(visdat)

resultados_cmff <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-17/resultados_cmff.csv")

###############################################################################################


glimpse(resultados_cmff)
head(resultados_cmff)
summary(resultados_cmff)


#Visualizar NA'S
vis_miss(resultados_cmff, sort_miss=TRUE)

# see total number of games by country
df_games <- resultados_cmff %>% group_by(equipo) %>% summarize(juegos = n())
# see total number of wins and losses by country
df_wins <- resultados_cmff %>% filter(resultado == "Victoria") %>% group_by(equipo) %>% summarize(victorias = n())
df_loss <- resultados_cmff %>% filter(resultado == "Derrota") %>% group_by(equipo) %>% summarize(derrotas = n())
# get percentage of games won
df_games_won <- inner_join(df_games, df_wins) %>% mutate(porcentaje_ganados = (victorias/juegos)*100)
# get net wins/losses by country
df_games <- left_join(df_games, df_wins) %>% mutate(victorias = replace_na(victorias, 0))
df_games <- left_join(df_games, df_loss) %>% mutate(derrotas = replace_na(derrotas, 0),
                                                    net = victorias-derrotas)

# capitalize country to be more consistent with FIFA France 2019 theme
df_games <- df_games %>% mutate(país = str_to_upper(equipo))

library(viridis)

#Win status per country
win.graph()
resultados_cmff %>% ggplot(aes(equipo,color=resultado, fill=resultado))+
  geom_bar(color = "#2b2b2b")+xlab("País")+
  facet_wrap(~resultado)+
  coord_flip()+
  labs(title="Copa Mundial de Futbol Femenil (1991-2019)", y="Número de juegos")+
  labs(subtitle = "Estadísticas por país") 


#Win percentage per country
win.graph()
df_games_won %>% mutate(equipo = fct_reorder(equipo,porcentaje_ganados)) %>% ggplot(aes(equipo, porcentaje_ganados))+
  geom_col()+
  scale_fill_viridis_c()+
  coord_flip()+xlab("País")+
  labs(title="Copa Mundial de Futbol Femenil (1991-2019)", y="Número de juegos")+
  labs(subtitle = "Porcentaje de juegos ganados por país (Calculado entre selecciones que han gando algún partido)")+theme_minimal()

h <- hist(resultados_cmff$goles, plot = TRUE) #histogram with Freedman-Diaconis rule for binwidth

summary(df_games$net)


win.graph()
df_games %>% mutate(país = fct_reorder(país,net)) %>% 
  ggplot(aes(país, net, fill=net < 0))+
  geom_col(width=0.95, color="white", size=0.2)+
  coord_flip()+
  scale_fill_manual( values=c("Darkblue","Skyblue"))+
  scale_y_continuous(expand=c(0,0), limits=c(-40,40), breaks=c(-30,-20,-10,0,10,20,30))+
  labs(title="Copa Mundial de Futbol Femenil", subtitle="De 1991-2019", y="VICTORIAS MENOS DERROTAS", fill="Resultado del juego")+theme_minimal()+
theme(
  legend.position = "none",
  panel.border = element_blank(),
) 