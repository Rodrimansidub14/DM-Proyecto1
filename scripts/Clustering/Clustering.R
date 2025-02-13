# Cargar librerías necesarias
library(tidyverse)      # Manipulación de datos y piping
library(hopkins)        # Para calcular el estadístico de Hopkins
library(fpc)            # Funciones relacionadas con clustering
library(factoextra)     # Visualización (VAT, método del codo, etc.)
library(parallelDist)   # Cálculo de distancias en paralelo

# Leer el dataset preprocesado (ajustar la ruta según corresponda)
movies_clean_scaled <- read.csv("C:/Users/rodri/Documents/Data-Mining/Proyecto 1/data/movies_clean_scaled.csv", 
                                stringsAsFactors = FALSE)
cat("Estructura del dataset preprocesado:\n")
str(movies_clean_scaled)
cat("Resumen del dataset preprocesado:\n")
summary(movies_clean_scaled)

# Seleccionar las variables numéricas (para clustering)
df_numeric <- movies_clean_scaled %>% select(where(is.numeric))

# Calcular el estadístico de Hopkins sobre el conjunto completo
# Seleccionar una muestra aleatoria para el cálculo de Hopkins
set.seed(123)
df_sample_hopkins <- df_numeric %>% sample_n(1120)  # 16 * 70 = 1120

# Calcular el estadístico de Hopkins sobre la muestra
hopkins_stat_sample <- hopkins(df_sample_hopkins)
cat("Estadístico de Hopkins (muestra):", sprintf("%.20f", hopkins_stat_sample), "\n")

# ---- Muestreo para análisis ----
# Tomamos 1120 observaciones (16*70) de forma aleatoria
n_sample <- 1120  
df_sample <- df_numeric %>% sample_n(n_sample)
df_sample_matrix <- as.matrix(df_sample)

# Calcular la matriz de distancias en paralelo (Euclidiana)
datos_dist_sample <- parDist(df_sample_matrix, method = "euclidean")

# Visualizar la matriz de distancias con fviz_dist (VAT)
plot_vat_sample <- fviz_dist(datos_dist_sample, 
                             show_labels = FALSE,
                             gradient = list(low = "#005D8F",    # Azul casi negro
                                             mid2 = "#5CC6FF",     # Azul medio
                                             mid3 = "#FFFFFF",     # Lavanda
                                             mid4 = "#E01522",     # Lavanda
                                             high = "#780000")) +   # Rojo oscuro
  ggtitle("VAT sobre muestra de 1120 observaciones") +
  theme_minimal() +
  scale_fill_gradientn(
    colors = c("#005D8F", "#5CC6FF", "#FFFFFF", "#E01522", "#780000"),
    values = scales::rescale(c(0, 2, 4, 6)),
    limits = c(0, 6),
    name = "Distancia"
)+
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )
print(plot_vat_sample)

# ---- Análisis detallado de distancias para Hopkins manual ----
# Usamos la misma muestra (o se puede tomar otra muestra aleatoria)
df_sample_analysis <- df_sample  # Ya es de 1120 observaciones
df_sample_analysis_matrix <- as.matrix(df_sample_analysis)

# Función para calcular la distancia euclidiana entre dos vectores
euclid_dist <- function(x, y) {
  sqrt(sum((x - y)^2))
}

# Para cada punto en la muestra, calcular la distancia al vecino más cercano (excluyéndose a sí mismo)
real_nn_distances <- numeric(n_sample)
for(i in 1:n_sample) {
  other_points <- df_sample_analysis_matrix[-i, , drop = FALSE]
  dists <- apply(other_points, 1, function(x) euclid_dist(df_sample_analysis_matrix[i, ], x))
  real_nn_distances[i] <- min(dists)
}

# Generar 1120 puntos aleatorios en el rango de cada variable (usando todo el dataset original)
ranges <- apply(df_numeric, 2, range)
random_points <- matrix(nrow = n_sample, ncol = ncol(df_numeric))
for(j in 1:ncol(df_numeric)) {
  random_points[, j] <- runif(n_sample, min = ranges[1, j], max = ranges[2, j])
}

# Para cada punto aleatorio, calcular la distancia mínima al conjunto completo de datos reales
df_numeric_matrix <- as.matrix(df_numeric)
random_nn_distances <- numeric(n_sample)
for(i in 1:n_sample) {
  dists <- apply(df_numeric_matrix, 1, function(x) euclid_dist(random_points[i, ], x))
  random_nn_distances[i] <- min(dists)
}

# Calcular manualmente el estadístico de Hopkins
hopkins_manual <- sum(random_nn_distances) / (sum(random_nn_distances) + sum(real_nn_distances))
cat("Estadístico de Hopkins (manual):", sprintf("%.10f", hopkins_manual), "\n")
