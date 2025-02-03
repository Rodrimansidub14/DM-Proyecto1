# ==============================================
# Proyecto CineVision Studios - Pregunta 3
# ==============================================

# 1. Cargar las bibliotecas necesarias
library(tidyverse)
library(nortest)    # Para pruebas de normalidad
library(ggplot2)    # Para gráficos avanzados
library(scales)     # Para formatear ejes

# 2. Crear directorios de salida si no existen
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

if (!dir.exists("outputs/figures")) {
  dir.create("outputs/figures")
}

# Crear subcarpetas para histogramas y barplots
if (!dir.exists("outputs/figures/histograms")) {
  dir.create("outputs/figures/histograms", recursive = TRUE)
}

if (!dir.exists("outputs/figures/barplots")) {
  dir.create("outputs/figures/barplots", recursive = TRUE)
}

if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables")
}

# 3. Leer el conjunto de datos limpio
movies_cleaned <- read_csv("data/movies_cleaned.csv")

# 4. Seleccionar variables cuantitativas relevantes
quantitative_vars <- movies_cleaned %>%
  select(
    budget, revenue, runtime, popularity, voteAvg, voteCount,
    genresAmount, actorsAmount, castWomenAmount, castMenAmount
  )

# 5. Resumen estadístico de las variables cuantitativas
summary_stats <- summary(quantitative_vars)
write.csv(as.data.frame(summary_stats), "outputs/tables/summary_statistics_quantitative_vars.csv", row.names = TRUE)

# 6. Calcular la desviación estándar para cada variable cuantitativa
sd_budget <- sd(movies_cleaned$budget, na.rm = TRUE)
sd_revenue <- sd(movies_cleaned$revenue, na.rm = TRUE)
sd_runtime <- sd(movies_cleaned$runtime, na.rm = TRUE)
sd_popularity <- sd(movies_cleaned$popularity, na.rm = TRUE)
sd_voteAvg <- sd(movies_cleaned$voteAvg, na.rm = TRUE)
sd_voteCount <- sd(movies_cleaned$voteCount, na.rm = TRUE)
sd_genresAmount <- sd(movies_cleaned$genresAmount, na.rm = TRUE)
sd_actorsAmount <- sd(movies_cleaned$actorsAmount, na.rm = TRUE)
sd_castWomenAmount <- sd(movies_cleaned$castWomenAmount, na.rm = TRUE)
sd_castMenAmount <- sd(movies_cleaned$castMenAmount, na.rm = TRUE)

# 7. Crear un dataframe con las desviaciones estándar
sd_df <- tibble(
  Variable = c("Presupuesto", "Ingresos", "Duración", "Popularidad", 
               "Calificación Promedio", "Número de Votos", "Cantidad de Géneros",
               "Cantidad de Actores", "Cantidad de Actrices", "Cantidad de Actores Masculinos"),
  Desviacion_Estandar = c(sd_budget, sd_revenue, sd_runtime, sd_popularity, 
                          sd_voteAvg, sd_voteCount, sd_genresAmount, 
                          sd_actorsAmount, sd_castWomenAmount, sd_castMenAmount)
)

# 8. Guardar las desviaciones estándar en un archivo CSV
write.csv(sd_df, "outputs/tables/desviaciones_estandar.csv", row.names = FALSE)

# =================================================================================
# Mejora de Gráficos - CineVision Studios
# =================================================================================

# 9. Definir una función para crear y guardar histogramas con estilo consistente
crear_y_guardar_histograma <- function(data, variable, titulo, x_label, carpeta_salida) {
  # Generar el histograma usando ggplot2
  p <- ggplot(data, aes_string(x = variable)) +
    geom_histogram(fill = "steelblue", color = "black", bins = 3, alpha = 0.7) +
    ggtitle(titulo) +
    xlab(x_label) +
    ylab("Frecuencia") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  # Definir el nombre del archivo de salida con nombre claro
  nombre_archivo <- paste0(carpeta_salida, "/", variable, "_histograma.png")
  
  # Guardar el histograma como archivo PNG
  ggsave(
    filename = nombre_archivo,
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
}

# 10. Definir las variables cuantitativas a graficar y sus títulos
variables_cuantitativas_nombres <- list(
  list(variable = "budget", titulo = "Distribución del Presupuesto de las Películas", x_label = "Presupuesto (USD)"),
  list(variable = "revenue", titulo = "Distribución de Ingresos de las Películas", x_label = "Ingresos (USD)"),
  list(variable = "runtime", titulo = "Distribución de la Duración de las Películas", x_label = "Duración (minutos)"),
  list(variable = "popularity", titulo = "Distribución de la Popularidad de las Películas", x_label = "Popularidad"),
  list(variable = "voteAvg", titulo = "Distribución de la Calificación Promedio de las Películas", x_label = "Calificación Promedio"),
  list(variable = "voteCount", titulo = "Distribución del Número de Votos de las Películas", x_label = "Número de Votos"),
  list(variable = "genresAmount", titulo = "Distribución de la Cantidad de Géneros por Película", x_label = "Cantidad de Géneros"),
  list(variable = "actorsAmount", titulo = "Distribución de la Cantidad de Actores por Película", x_label = "Cantidad de Actores"),
  list(variable = "castWomenAmount", titulo = "Distribución de la Cantidad de Actrices por Película", x_label = "Cantidad de Actrices"),
  list(variable = "castMenAmount", titulo = "Distribución de la Cantidad de Actores Masculinos por Película", x_label = "Cantidad de Actores Masculinos")
)

# 11. Generar y guardar los histogramas
for (var in variables_cuantitativas_nombres) {
  crear_y_guardar_histograma(
    data = movies_cleaned,
    variable = var$variable,
    titulo = var$titulo,
    x_label = var$x_label,
    carpeta_salida = "outputs/figures/histograms"
  )
  
  # Opcional: Imprimir un mensaje de confirmación
  cat("Histograma de", var$variable, "guardado en outputs/figures/histograms\n")
}

# =================================================================================
# Pruebas de Normalidad
# =================================================================================

# 12. Definir una función para realizar pruebas de normalidad y guardar los resultados
test_normality_save <- function(variable, var_name) {
  cat("\n### Pruebas de Normalidad para", var_name, "###\n")
  
  # Debido a que Shapiro-Wilk no es adecuado para n > 5000, tomamos una muestra de 5000
  set.seed(123)  # Para reproducibilidad
  sample_var <- sample(variable, 5000, replace = FALSE)
  
  # Shapiro-Wilk
  shapiro_result <- tryCatch({
    shapiro.test(sample_var)
  }, error = function(e) {
    list(W = NA, p.value = NA)
  })
  
  cat("Shapiro-Wilk Test:\n")
  print(shapiro_result)
  
  # Kolmogorov-Smirnov
  ks_result <- tryCatch({
    ks.test(sample_var, "pnorm", mean = mean(sample_var, na.rm = TRUE), sd = sd(sample_var, na.rm = TRUE))
  }, error = function(e) {
    list(statistic = NA, p.value = NA)
  })
  
  cat("Kolmogorov-Smirnov Test:\n")
  print(ks_result)
  
  # Lilliefors (Kolmogorov-Smirnov sin parámetros poblacionales)
  lillie_result <- tryCatch({
    lillie.test(sample_var)
  }, error = function(e) {
    list(statistic = NA, p.value = NA)
  })
  
  cat("Lilliefors Test:\n")
  print(lillie_result)
}

# 13. Guardar las pruebas de normalidad en un archivo de texto
sink("outputs/tables/pruebas_normalidad.txt")

# 14. Aplicar la función a cada variable cuantitativa
for (var in variables_cuantitativas_nombres) {
  test_normality_save(movies_cleaned[[var$variable]], var$titulo)
}

# Cerrar el archivo de salida
sink()

# =================================================================================
# Tablas de Frecuencia para Variables Cualitativas
# =================================================================================

# 15. Seleccionar variables cualitativas relevantes
qualitative_vars <- movies_cleaned %>%
  select(
    genre1, originalLanguage, productionCountry, video, director, actors
  )

# 16. Definir una función para generar y guardar tablas de frecuencia como CSV
save_frequency_table <- function(variable, var_name) {
  freq_table <- as.data.frame(table(variable, useNA = "ifany"))
  colnames(freq_table) <- c(var_name, "Frecuencia")
  write.csv(freq_table, paste0("outputs/tables/frecuencia_", var_name, ".csv"), row.names = FALSE)
}

# 17. Aplicar la función a cada variable cualitativa seleccionada
save_frequency_table(movies_cleaned$genre1, "Género_Principal")
save_frequency_table(movies_cleaned$originalLanguage, "Idioma_Original")
save_frequency_table(movies_cleaned$productionCountry, "Pais_de_Produccion")
save_frequency_table(movies_cleaned$video, "Videos_Promocionales")
save_frequency_table(movies_cleaned$director, "Director")
save_frequency_table(movies_cleaned$actors, "Actores")

# =================================================================================
# Generar Gráficos de Barras para Variables Cualitativas Relevantes
# =================================================================================

# 18. Definir una función para crear y guardar gráficos de barras para variables cualitativas
crear_y_guardar_barplot <- function(data, variable, titulo, x_label, y_label, carpeta_salida, top_n = 10) {
  # Calcular las frecuencias y seleccionar las principales categorías
  freq_data <- data %>%
    group_by_at(variable) %>%
    summarise(Frecuencia = n()) %>%
    arrange(desc(Frecuencia)) %>%
    slice_max(order_by = Frecuencia, n = top_n)
  
  # Generar el gráfico de barras
  p <- ggplot(freq_data, aes_string(x = reorder(variable, -Frecuencia), y = "Frecuencia")) +
    geom_bar(stat = "identity", fill = "coral", color = "black", alpha = 0.7) +
    ggtitle(titulo) +
    xlab(x_label) +
    ylab(y_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Definir el nombre del archivo de salida
  nombre_archivo <- paste0(carpeta_salida, "/", variable, "_barplot.png")
  
  # Guardar el gráfico como archivo PNG
  ggsave(
    filename = nombre_archivo,
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# 19. Generar y guardar gráficos de barras para variables cualitativas seleccionadas
crear_y_guardar_barplot(
  data = movies_cleaned,
  variable = "genre1",
  titulo = "Top 10 Géneros Principales de las Películas",
  x_label = "Género Principal",
  y_label = "Frecuencia",
  carpeta_salida = "outputs/figures/barplots",
  top_n = 10
)

crear_y_guardar_barplot(
  data = movies_cleaned,
  variable = "originalLanguage",
  titulo = "Top 10 Idiomas Originales de las Películas",
  x_label = "Idioma Original",
  y_label = "Frecuencia",
  carpeta_salida = "outputs/figures/barplots",
  top_n = 10
)

crear_y_guardar_barplot(
  data = movies_cleaned,
  variable = "productionCountry",
  titulo = "Top 10 Países de Producción de las Películas",
  x_label = "País de Producción",
  y_label = "Frecuencia",
  carpeta_salida = "outputs/figures/barplots",
  top_n = 10
)

crear_y_guardar_barplot(
  data = movies_cleaned,
  variable = "director",
  titulo = "Top 10 Directores con Más Películas",
  x_label = "Director",
  y_label = "Frecuencia",
  carpeta_salida = "outputs/figures/barplots",
  top_n = 10
)

crear_y_guardar_barplot(
  data = movies_cleaned,
  variable = "actors",
  titulo = "Top 10 Actores con Más Películas",
  x_label = "Actor",
  y_label = "Frecuencia",
  carpeta_salida = "outputs/figures/barplots",
  top_n = 10
)

crear_y_guardar_barplot(
  data = movies_cleaned,
  variable = "video",
  titulo = "Frecuencia de Videos Promocionales",
  x_label = "Video Promocional",
  y_label = "Frecuencia",
  carpeta_salida = "outputs/figures/barplots",
  top_n = 2  # Solo TRUE y FALSE
)

# =================================================================================
# Fin del Script para Pregunta 3
# ==================================================================================
