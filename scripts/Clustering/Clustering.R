#librerías necesarias
library(tidyverse)      # Manipulación de datos y piping
library(hopkins)        # Estadístico de Hopkins
library(fpc)            # Funciones de clustering
library(factoextra)     # Visualización (método del codo, VAT, etc.)
library(parallelDist)   # Cálculo de distancias en paralelo
library(ggrepel)        # Etiquetas en gráficos
library(GGally)         # Gráficos de pares
library(flexclust)      # kcca y FeatureImpCluster
library(data.table)     # FeatureImpCluster
library(dplyr)
library(cluster)
library(ggplot2)
# Leer el dataset preprocesado y transformado
movies_clean_transformed <- read.csv("C:/Users/rodri/Documents/Data-Mining/Proyecto 1/data/movies_clean_transformed.csv", 
                                     stringsAsFactors = FALSE)
cat("Estructura del dataset preprocesado y transformado:\n")
str(movies_clean_transformed)
cat("Resumen del dataset preprocesado y transformado:\n")
summary(movies_clean_transformed)

# Seleccionar las variables relevantes para clustering
# Variables elegidas: budget_log, revenue_log, popularity_log, voteAvg, actorsPopularity, runtime y releaseYear
df_numeric <- movies_clean_transformed %>% 
  select(budget_log, revenue_log, popularity_log, voteAvg, actorsPopularity, runtime, releaseYear)

# Calcular el estadístico de Hopkins sobre una muestra del conjunto
set.seed(123)
df_sample_hopkins <- df_numeric %>% sample_n(1120)
hopkins_stat_sample <- hopkins(df_sample_hopkins)
cat("Estadístico de Hopkins (muestra):", sprintf("%.20f", hopkins_stat_sample), "\n")

# ---- Muestreo para análisis ----
# Tomamos 1120 observaciones de forma aleatoria
n_sample <- 1120  
df_sample <- df_numeric %>% sample_n(n_sample)
df_sample_matrix <- as.matrix(df_sample)

# Calcular la matriz de distancias (Euclidiana) en paralelo
# Normalización Min-Max para evitar valores extremos
datos_dist_sample <- (datos_dist_sample - min(datos_dist_sample)) / 
  (max(datos_dist_sample) - min(datos_dist_sample))

# Verificar rango después de normalizar
rango_dist <- range(datos_dist_sample)
print(rango_dist)

# Visualizar la matriz de distancias con fviz_dist (VAT)
plot_vat_sample <- fviz_dist(datos_dist_sample, 
                             show_labels = FALSE,
                             gradient = list(low = "#005D8F",    
                                             mid2 = "#5CC6FF",    
                                             mid3 = "#FFFFFF",     
                                             mid4 = "#E01522",    
                                             high = "#780000")) +  
  ggtitle("VAT sobre muestra de 1120 observaciones") +
  theme_minimal() +
  scale_fill_gradientn(
    colors = c("#005D8F", "#5CC6FF", "#FFFFFF", "#E01522", "#780000"),
    values = scales::rescale(c(
      quantile(datos_dist_sample, 0.01),  
      quantile(datos_dist_sample, 0.25),  
      quantile(datos_dist_sample, 0.75),  
      quantile(datos_dist_sample, 0.99)   
    )),
    name = "Distancia"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

# Mostrar la gráfica
print(plot_vat_sample)

## Determinación del número óptimo de clusters: Método del Codo
set.seed(123)
elbow_plot <- fviz_nbclust(df_numeric, kmeans, method = "wss") +
  labs(subtitle = "Método del Codo")
print(elbow_plot)

# Calcular el WSS para distintos números de clusters
set.seed(123)
wss <- numeric(10)
for (i in 1:10) {
  km_tmp <- kmeans(df_numeric, centers = i, nstart = 25)
  wss[i] <- sum(km_tmp$withinss)
}
plot(1:10, wss, type = "b", 
     xlab = "Número de Clústeres", 
     ylab = "Suma de cuadrados intra-grupo",
     main = "Gráfica del Codo")

## Aplicar K-means (por ejemplo, con 4 clusters)
set.seed(123)
km <- kmeans(df_numeric, centers = 4, iter.max = 100, nstart = 25)

# Agregar el grupo obtenido al dataset original
movies_clean_transformed$grupo <- km$cluster

# Imprimir resumen del modelo k-means
cat("Resumen del modelo de K-medias:\n")
print(km)

## Grafico de clusters

# Supongamos que quieres graficar solo 500 puntos
set.seed(123)
idx <- sample(seq_len(nrow(df_numeric)), 45)
df_sub <- df_numeric[idx, ]
clusters_sub <- km$cluster[idx]

# Graficar con plotcluster usando la muestra
plotcluster(as.matrix(df_sub), 
            clusters_sub, 
            method = "dc", 
            clnum = TRUE, 
            main = "Ubicación de los Clusters")

# Visualizar los clusters
set.seed(123)
n_sample <- 85
idx <- sample(seq_len(nrow(df_numeric)), n_sample)
df_sub <- df_numeric[idx, ]

# 2. Aplicar K-means a la muestra
km_sub <- kmeans(df_sub, centers = 4, nstart = 25)

# 3. Visualizar los clusters con fviz_cluster
fviz_cluster(km_sub,
             data = df_sub,
             geom = "point",         # Representar cada punto
             ellipse.type = "norm",  # Elipses basadas en la varianza de cada cluster
             main = "Visualización de Clusters (Muestra de 500 Puntos)")


## Analisis de los grupos
km <- kmeans(df_numeric, centers = 4, iter.max = 100, nstart = 25)


km$size
km$withinss

## caardinalidad vs magnitud

# Crear un data frame con la cardinalidad (size) y la magnitud (withinss)
m <- data.frame(
  withinss = km$withinss,
  size     = km$size,
  cluster  = factor(seq_along(km$size))  # Identificar el número de cluster
)

# Graficar cardinalidad vs. magnitud
ggplot(m, aes(x = size, y = withinss)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text_repel(aes(label = cluster), color = "black") +
  labs(
    x = "Cardinalidad (size)",
    y = "Magnitud (withinss)",
    title = "Cardinalidad vs. Magnitud de los Clusters"
  ) +
  theme_minimal()


## Pares



# Convertir el grupo en factor
movies_clean_transformed$grupo <- as.factor(movies_clean_transformed$grupo)

# Crear subconjunto con las variables y el grupo
df_plot <- movies_clean_transformed[, c("budget_log", "revenue_log", "popularity_log", "grupo")]

# Gráfico de pares del primer grupo de variables
ggpairs(df_plot, aes(color = grupo),
        progress = FALSE) +
  ggtitle("Gráfico de Pares - Grupo 1")

# Crear subconjunto para el segundo grupo
df_plot2 <- movies_clean_transformed[, c("voteAvg", "actorsPopularity", "runtime", "grupo")]

# Gráfico de pares del segundo grupo de variables
ggpairs(df_plot2, aes(color = grupo),
        progress = FALSE) +
  ggtitle("Gráfico de Pares - Grupo 2")



## 2. Calcular la importancia de variables
# =============================
# 1. CARDINALIDAD VS. MAGNITUD
# =============================
m <- data.frame(
  withinss = km$withinss,
  size     = km$size,
  cluster  = factor(seq_along(km$size))  # Identifica cada cluster
)

library(ggplot2)
library(ggrepel)

ggplot(m, aes(x = size, y = withinss)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text_repel(aes(label = cluster), color = "black") +
  labs(
    x = "Cardinalidad (size)",
    y = "Magnitud (withinss)",
    title = "Cardinalidad vs. Magnitud de los Clusters"
  ) +
  theme_minimal()

# =============================
# 2. GRÁFICOS DE PARES CON GGPAIRS
# =============================
library(GGally)

# Asegurarse de que la variable de grupo sea factor
movies_clean_transformed$grupo <- as.factor(movies_clean_transformed$grupo)

# Crear subconjunto para el primer grupo de variables
df_plot1 <- movies_clean_transformed[, c("budget_log", "revenue_log", "popularity_log", "grupo")]
ggpairs(df_plot1, aes(color = grupo), progress = FALSE) +
  ggtitle("Gráfico de Pares - Grupo 1")

# Crear subconjunto para el segundo grupo de variables
df_plot2 <- movies_clean_transformed[, c("voteAvg", "actorsPopularity", "runtime", "grupo")]
ggpairs(df_plot2, aes(color = grupo), progress = FALSE) +
  ggtitle("Gráfico de Pares - Grupo 2")

# =============================

## Metodo de la silueta K-means
# Definir un tamaño de muestra fijo
# Asegurar reproducibilidad
set.seed(123)

# Tomar muestra del conjunto de datos
n_sample <- 85
df_sample <- df_numeric %>% sample_n(n_sample)

# Aplicar k-means sobre la muestra
km <- kmeans(df_sample, centers = 4, iter.max = 100, nstart = 25)

# Crear la visualización
# Asegurar reproducibilidad
set.seed(123)

# Tomar muestra del conjunto de datos
n_sample <- 85
df_sample <- df_numeric %>% sample_n(n_sample)

# Aplicar k-means sobre la muestra
km <- kmeans(df_sample, centers = 4, iter.max = 100, nstart = 25)

# Calcular la silueta para el modelo k-means en la muestra
library(cluster)
silkm <- silhouette(km$cluster, dist(df_sample))
silhouette_promedio <- mean(silkm[, 3])
cat("Silhouette promedio:", silhouette_promedio, "\n")

# Graficar el análisis de la silueta
plot(silkm, cex.names = 0.8, col = 1:6, main = "Análisis de la Silueta - Muestra fija")




### Clustering Jerarquico. 


# Calcular la matriz de distancias
### Clustering Jerárquico

# 1. Calcular la matriz de distancias (usando la muestra ya seleccionada)
# Usamos la métrica Euclidiana
datos_dist <- dist(df_sample, method = "euclidean")

# 2. Realizar el agrupamiento jerárquico
# Se utiliza el método "ward.D2" para minimizar la varianza intra-grupo
hc <- hclust(datos_dist, method = "ward.D2")

# 3. Visualizar el dendrograma
plot(hc, main = "Dendrograma - Clustering Jerárquico", xlab = "", sub = "", cex = 0.6)
# Dibujar el corte en el dendrograma (por ejemplo, para 3 grupos)
rect.hclust(hc, k = 4, border = "red")
fviz_dend(hc, k = 4, rect = TRUE, cex = 0.5)

# 4. Cortar el dendrograma para asignar cada observación a un grupo
groups_hc <- cutree(hc, k = 4)
# Agregar la asignación de grupos al dataset de la muestra
df_sample$grupo_hc <- groups_hc
# Mostrar la tabla de frecuencias de los grupos
table(df_sample$grupo_hc)
# media de cada grupo
aggregate(df_sample, by = list(grupo_hc = groups_hc), FUN = mean)
fviz_dend(hc, k = 4, horiz = TRUE, cex = 0.5)

#dendograma radial
set.seed(123)
fviz_dend(hc, k = 4, cex = 0.4, type = "circular", color_labels_by_k = TRUE)

## dendograma filogenetico

fviz_dend(hc, k = 4, cex = 0.7, type = "phylogenic", color_labels_by_k = TRUE,repel = T)


##grupo más cohesivo

# 5. Evaluar la calidad del agrupamiento usando el método de la silueta
library(cluster)
silhc <- silhouette(groups_hc, datos_dist)
sil_promedio_hc <- mean(silhc[, 3])
cat("Silhouette promedio (Clustering Jerárquico):", sil_promedio_hc, "\n")

# 6. Visualizar el análisis de la silueta
plot(silhc, main = "Análisis de la Silueta - Clustering Jerárquico", cex.names = 0.8, col = 1:3)
