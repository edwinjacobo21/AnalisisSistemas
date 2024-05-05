library(readxl)
EJERCICIO4 <- read_excel("C:/Users/Edwin/Desktop/EJERCICIO4.xlsx")
EJERCICIO4


datos <- EJERCICIO4
k.means.fit <-kmeans(datos[,2:3], 3, nstart = 10)
k.means.fit 
k.means.fit$centers
k.means.fit$ifault

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

d2 <- scale(datos[,2:3])
rownames(d2) <- datos$Variables
fviz_nbclust(x = d2, FUNcluster = kmeans, method = "wss", k.max = 9, 
             diss = get_dist(d2, method = "euclidean"), nstart = 50)


d2f=data.frame(d2)
km_clusters <- kmeans(x = d2f, centers = 3, nstart = 50)

fviz_cluster(object = km_clusters, data = d2f, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,
             pointsize=0.5,outlier.color="darkred") +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +  theme(legend.position = "none")      

require(cluster)
pam.res <- pam(d2f, 3)

fviz_cluster(pam.res, geom = "point", ellipse.type = "norm",
             show.clust.cent = TRUE,star.plot = TRUE)+
  labs(title = "Resultados clustering K-means")+ theme_bw()


pca <- prcomp(EJERCICIO4[,2:3])
df.pca <- pca$x
df.pca

kc <- kmeans(df.pca[,1:2], 2)
fviz_pca_biplot(pca, label="var", habillage=as.factor(kc$cluster)) +
  labs(color=NULL) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "orange"))
