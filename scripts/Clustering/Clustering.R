# Cargar librerías necesarias
library(tidyverse)   # Para manipulación de datos y piping
library(hopkins)     # Para calcular el estadístico de Hopkins
library(fpc)         # Para funciones relacionadas con clustering
library(factoextra)  # Para visualización (incluyendo VAT y método del codo)
library(parallelDist)

# Leer el dataset preprocesado (ajustar la ruta según corresponda)
movies_clean_scaled <- read.csv("C:/Users/rodri/Documents/Data-Mining/Proyecto 1/data/movies_clean_scaled.csv", 
                                stringsAsFactors = FALSE)

# Seleccionar las variables numéricas (utilizadas para clustering)
df_numeric <- movies_clean_scaled %>% select(where(is.numeric))

# Calcular el estadístico de Hopkins
set.seed(123)
# Calcular el estadístico de Hopkins sobre todo el conjunto de datos
hopkins_stat_full <- hopkins(df_numeric)
cat("Estadístico de Hopkins (conjunto completo):", hopkins_stat_full, "\n")



# Seleccionar una muestra de, por ejemplo, 500 observaciones (ajusta este número según los recursos de tu máquina)
n_sample <- 2500  
df_sample <- df_numeric[sample(nrow(df_numeric), n_sample), ]

# Calcular la matriz de distancias para la muestra
datos_dist_sample <- parDist(as.matrix(df_sample), method = "euclidean")

# Generar el mapa de calor (VAT) para la muestra sin etiquetas
plot_vat_sample <- fviz_dist(datos_dist_sample, 
                             show_labels = FALSE, 
                             gradient = list(low = "#623CEA", mid = "white", high = "#FFCA3A")) +
  ggtitle("VAT sobre muestra de datos")
print(plot_vat_sample)
