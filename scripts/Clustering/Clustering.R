# Cargar librerías necesarias
library(tidyverse)      # Para manipulación de datos y piping
library(hopkins)        # Para calcular el estadístico de Hopkins
library(fpc)            # Para funciones relacionadas con clustering
library(factoextra)     # Para visualización (VAT y método del codo)
library(parallelDist)   # Para calcular la matriz de distancias en paralelo

# Leer el dataset preprocesado (ajustar la ruta según corresponda)
movies_clean_scaled <- read.csv("C:/Users/rodri/Documents/Data-Mining/Proyecto 1/data/movies_clean_scaled.csv", 
                                stringsAsFactors = FALSE)
cat("Estructura del dataset preprocesado:\n")
str(movies_clean_scaled)
cat("Resumen del dataset preprocesado:\n")
summary(movies_clean_scaled)
# Seleccionar las variables numéricas (utilizadas para clustering)
df_numeric <- movies_clean_scaled %>% 
  select(where(is.numeric))

# Calcular el estadístico de Hopkins sobre el conjunto completo
set.seed(123)
hopkins_stat_full <- hopkins(df_numeric)
cat("Estadístico de Hopkins (conjunto completo):", sprintf("%.20f", hopkins_stat_full), "\n")

# Seleccionar una muestra representativa para reducir carga computacional
n_sample <- 1000  # Ajusta este número según los recursos de tu máquina
df_sample <- df_numeric[sample(nrow(df_numeric), n_sample), ]

# Convertir la muestra a matriz (optimiza cálculos numéricos)
df_sample_matrix <- as.matrix(df_sample)

# Calcular la matriz de distancias en paralelo (método Euclidiano)
datos_dist_sample <- parDist(df_sample_matrix, method = "euclidean")

# Generar el mapa VAT usando fviz_dist (sin etiquetas)
plot_vat_sample <- fviz_dist(datos_dist_sample, 
                             show_labels = FALSE, 
                             gradient = list(low = "#623CEA", mid = "blue", high = "#FFCA3A")) +
  ggtitle("VAT sobre muestra de datos")
print(plot_vat_sample)
# -------------------------------
# Análisis detallado de distancias para explicar el Hopkins cercano a 1
# -------------------------------

# 1. Seleccionar una muestra de 500 observaciones para el análisis
n_sample <- 8500
df_sample <- df_numeric[sample(nrow(df_numeric), n_sample), ]
df_sample_matrix <- as.matrix(df_sample)

# 2. Definir una función para calcular la distancia euclidiana entre dos vectores
euclid_dist <- function(x, y) {
  sqrt(sum((x - y)^2))
}

# 3. Para cada punto de la muestra, calcular la distancia al vecino más cercano en la muestra
# (excluyendo a sí mismo)
real_nn_distances <- numeric(n_sample)
for(i in 1:n_sample) {
  # Excluir el punto i para calcular la distancia a los demás
  other_points <- df_sample_matrix[-i, , drop = FALSE]
  dists <- apply(other_points, 1, function(x) euclid_dist(df_sample_matrix[i, ], x))
  real_nn_distances[i] <- min(dists)
}

# 4. Generar n_sample puntos aleatorios en el mismo rango de cada variable.
# Obtener los rangos de cada columna en el dataset original
ranges <- apply(df_numeric, 2, range)

# Inicializar una matriz para los puntos aleatorios
random_points <- matrix(nrow = n_sample, ncol = ncol(df_numeric))
for(j in 1:ncol(df_numeric)) {
  random_points[, j] <- runif(n_sample, min = ranges[1, j], max = ranges[2, j])
}

# 5. Para cada punto aleatorio, calcular la distancia al vecino más cercano en el dataset real completo
df_numeric_matrix <- as.matrix(df_numeric)
random_nn_distances <- numeric(n_sample)
for(i in 1:n_sample) {
  dists <- apply(df_numeric_matrix, 1, function(x) euclid_dist(random_points[i, ], x))
  random_nn_distances[i] <- min(dists)
}


# 7. Calcular manualmente el estadístico de Hopkins usando la fórmula:
# Hopkins = sum(random_nn_distances) / (sum(random_nn_distances) + sum(real_nn_distances))
hopkins_manual <- sum(random_nn_distances) / (sum(random_nn_distances) + sum(real_nn_distances))
cat("Estadístico de Hopkins (manual):", sprintf("%.10f", hopkins_manual), "\n")

