library(tidyverse)
library(readr)
library(ggridges)
library(forcats)

resultados_cmff <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-17/resultados_cmff.csv")

wwc_sum <- resultados_cmff %>%
  group_by(equipo) %>%
  summarise(
    win_game_ratio = round(sum(resultado == "Victoria") / n(), 2)
  ) %>%
  arrange(
    -win_game_ratio
  ) %>%
  top_n(20, win_game_ratio)

min_yr <- min(resultados_cmff$anio)
max_yr <- max(resultados_cmff$anio)

win.graph()
ggplot(wwc_sum,
       aes(x = fct_reorder(equipo, win_game_ratio),
           y = win_game_ratio)) +
  geom_point(size = 5, color = "red",alpha=0.8) +
  geom_segment(aes(xend = fct_reorder(equipo, win_game_ratio),
                   y = 0, yend = win_game_ratio),
               size = 1) +
  coord_flip() +
  labs(
    x = "", y = "",
    title = "Top 20 equipos del Mundial de Futbol Femenil por número de victorias / número de juegos",
    subtitle = glue::glue("Años: 1991-2019"),
    caption = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

###############################################################################################


