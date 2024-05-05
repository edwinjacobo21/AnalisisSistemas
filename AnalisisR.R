#Un analista de mercado trabajando para una empresa que quiere entender mejor las preferencias de los 
#consumidores en una pequeña ciudad. La empresa está interesada particularmente en cómo los consumidores se 
#relacionan con las marcas y las tiendas, considerando que estas relaciones pueden influir en las estrategias de 
#marketing y promoción. Para investigar esto, se ha realizado una encuesta a 10 personas, evaluando dos aspectos 
#principales: la lealtad hacia las marcas y la lealtad hacia las tiendas locales. Cada encuestado ha calificado 
#su lealtad en una escala de 0 a 10 para ambas dimensiones.


#Después de realizar el análisis de cluster en los datos de la encuesta sobre la lealtad de los consumidores 
#hacia marcas y tiendas locales, identificamos varios grupos distintos. Algunos consumidores muestran alta 
#lealtad tanto a las marcas como a las tiendas locales, mientras que otros son leales solo a las marcas o solo 
#a las tiendas locales. También encontramos un grupo de consumidores con baja lealtad en ambas dimensiones.

#Dado que la empresa busca comprender cómo se relacionan estas dos dimensiones de lealtad entre los 
#consumidores de la ciudad, ¿cómo podrías organizar y analizar los datos recopilados para identificar
#posibles patrones o tendencias en el comportamiento de los consumidores?s



library(readxl)
EJERCICIO4 <- read_excel("C:/Users/Edwin/Desktop/EJERCICIO4.xlsx")
EJERCICIO4


datos <- EJERCICIO4
k.means.fit <-kmeans(datos[,2:3], 3, nstart = 10)
k.means.fit 
k.means.fit$centers
k.means.fit$ifault

#Los resultados muestran que el análisis de clustering sugiere la presencia de 3 clusters óptimos. 
#Se proporciona información sobre el tamaño de cada cluster, los valores promedio de las variables en cada 
#cluster. Este análisis ayuda a entender cómo se agrupan los datos y qué características comparten los elementos 
#dentro de cada grupo.

grupos=k.means.fit$cluster
table(datos$Variables,grupos)


library(dplyr)
dif=data.frame(datos,grupos)
dif=data.frame(dif) %>% 
  mutate(grupos=dplyr::recode(grupos,
                              "10"="J",
                              "9"="I",
                              "8"="H",
                              "7"="G",
                              "6"="F",
                              "5"="E",
                              "4"="D",
                              "3"="C",
                              "2"="B",
                              "1"="A")) 
table(dif$grupos,dif$Variables)

# Combinación K-means y PCA

#El proceso de agrupación de variables puede volverse complejo a medida que aumenta el número 
#de variables, lo que dificulta el análisis de los datos en su totalidad. Por esta razón, el Análisis 
#de Componentes Principales (PCA) se convierte en una herramienta crucial. El PCA identifica las variables
#que contribuyen más a la variabilidad de los datos y permite trabajar solo con estas variables, simplificando 
#así el análisis y facilitando la interpretación de los resultados.




d2 <- scale(datos[,2:3])
rownames(d2) <- datos$Variables
fviz_nbclust(x = d2, FUNcluster = kmeans, method = "wss", k.max = 9, 
             diss = get_dist(d2, method = "euclidean"), nstart = 50)


#Esta gráfica ilustra la cantidad óptima de clusters sugerida por los datos actuales. 
#Aunque el análisis permite una clasificación de hasta 9 clusters, se ha demostrado que utilizar tres 
#clusters puede ser adecuado para representar los patrones distintivos en los datos. Esto sugiere que 
#una subdivisión más allá de tres clusters podría no ser necesaria para capturar la estructura subyacente de
#los datos de manera efectiva.


d2f=data.frame(d2)
km_clusters <- kmeans(x = d2f, centers = 3, nstart = 50)

fviz_cluster(object = km_clusters, data = d2f, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,
             pointsize=0.5,outlier.color="darkred") +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +  theme(legend.position = "none")  



#En esta grafia se combinan entonces el método K-Means con PCA. Los dos componentes graficados 
#representan 95.3% de la variabilidad de los datos, por lo que es más que suficiente trabajar con 
#estos dos elementos.

require(cluster)
pam.res <- pam(d2f, 3)

fviz_cluster(pam.res, geom = "point", ellipse.type = "norm",
             show.clust.cent = TRUE,star.plot = TRUE)+
  labs(title = "Resultados clustering K-means")+ theme_bw()


pca <- prcomp(EJERCICIO4[,2:3])
df.pca <- pca$x
df.pca

kc <- kmeans(df.pca[,1:2], 2)
fviz_pca_biplot(pca, label="var", habillage=as.factor(kc$cluster)) 
labs(color=NULL) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "orange"))

#Gracias a esta gráfica ahora se sabe que la longitud de la variable marca es 
#la variable más representada por el primer componente. En el caso del componente dos, 
#la variable tienda es la que está menos representada. 
#ademas la gráfica muestra cómo los datos se agrupan en un plano bidimensional generado por PCA. 
#Los puntos representan las observaciones, los colores indican los grupos identificados por k-means y 
#las flechas muestran la importancia de las variables.


