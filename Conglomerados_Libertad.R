library(tidyverse)
library(readr)
library(viridis)
library(ggthemes)

libertad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-14/libertad.csv")
libertad18 <- libertad %>%filter(anio == 2016)
table(libertad18$region)
attach(libertad18)
libertad18_latino <- libertad18 %>%filter(region=="Norteamérica" | region=="Latinoamérica y el Caribe")

libertad18_latino<-as.data.frame(libertad18_latino)
row.names(libertad18_latino)<-libertad18_latino$pais
libertad18_latino<- select(libertad18_latino, -libertad_humana_ranking, -libertad_personal_ranking,-libertad_economica_ranking) # Forma 1

#Matriz de Correlacion
library(corrplot)
library(viridis) # Paquete para paletas de colores

correlacionC<-cor(libertad18_latino[,5:7]) #Obtenemos la matriz de correlación
win.graph()
corrplot(correlacionC, method = "circle", tl.col = "black",col =(inferno(50)))
corrplot(correlacionC, method = "circle", type = "upper",tl.col = "black",col =inferno(50))
corrplot(correlacionC, method = "circle", type = "lower",tl.col = "black",col =inferno(50))
corrplot(correlacionC, method = "color", type = "lower",tl.col = "black",col =(inferno(50)))


#Escalamos las variables
datos<-scale(libertad18_latino[,5:7])

#Distancia Euclideana
mat_dist <- dist(x = datos, method = "euclidean") 
round(as.matrix(mat_dist)[1:5, 1:5], 2)

#Dendogramas con ligas complete, average,single,ward,centroid
hc_completa <- hclust(d = mat_dist, method = "complete") 
hc_promedio <- hclust(d = mat_dist, method = "average") 
hc_simple <- hclust(d = mat_dist, method = "single") 
hc_ward <- hclust(d = mat_dist, method = "ward.D2") 
hc_centroide<-hclust(d = mat_dist, method = "centroid") 

c1<-cor(x = mat_dist, cophenetic(hc_completa))
c2<-cor(x = mat_dist, cophenetic(hc_promedio))
c3<-cor(x = mat_dist, cophenetic(hc_simple))
c4<-cor(x = mat_dist, cophenetic(hc_ward))
c5<-cor(x = mat_dist, cophenetic(hc_centroide))

cors<-c(c1,c2,c3,c4,c5)
cors # La mas cercana al valor de 1 es la liga promedio

library(factoextra)
library(cluster)

######
win.graph()
fviz_dend(hc_promedio, cex = 0.6, k = 3, # Cut in four groups
          main = "Libertad en América 2016\nDendograma-Promedio con distancia euclideana" ,ylab = "",
          k_colors =  "lancet",color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border =  "lancet", 
          rect_fill = TRUE,
          ggtheme = theme_minimal())


#Dendrograma circular

fviz_dend(hc_promedio, cex = 0.6, k = 3, 
          main = "Libertad en América\nDendograma-Promedio",
          k_colors = "lancet", type = "circular",
          ggtheme = theme_minimal())



#k-means
#Numero óptimo de clusters

#Suma de cuadrados
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", diss = dist(datos, method = "euclidean")) 
+geom_vline(xintercept = 6, linetype = 1) +labs(title = "Número óptimo de clusters")

#Silhouette
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "silhouette") + labs(title = "Número óptimo de clusters")

#Gap_stat
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "gap_stat", nboot = 500, verbose = FALSE, nstart = 25) + labs(title = "Número óptimo de clusters")


#Recopilacion de varios indices
#La función NbClust() del paquete NbClust incorpora 30 índices distintos, 
#dando la posibilidad de calcularlos todos en un único paso. 
#Esto último es muy útil, ya que permite identificar el valor en el que coinciden 
#más índices, aportando seguridad de que se está haciendo una buena elección.

library(NbClust) 
numero_clusters <- NbClust(data = datos, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "alllong")
#Comparando los indices concluyen que sean 2 los cluster
fviz_nbclust(numero_clusters)

set.seed(123) 
km_clusters <- kmeans(x = mat_dist, centers = 3, nstart = 25)
win.graph()
fviz_cluster(object = km_clusters, data = datos, show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,main = "Libertad en América 2016\nK-medias con distancia euclideana") +
  theme_minimal() 
