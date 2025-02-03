# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

peliculas <- read_csv("data/movies_cleaned.csv")

##4.2. (3 puntos) ¿Cuáles son las 10 películas que más ingresos tuvieron?
# Leer el archivo y asignarlo al objeto "peliculas"

# Filtrar las 10 películas con mayores ingresos usando "movies_cleaned" y la columna "revenue"
movies_top10 <- movies_cleaned %>%
  arrange(desc(revenue)) %>%  # Ordena de mayor a menor según "revenue"
  head(10) %>%                # Selecciona las primeras 10 películas
  arrange(revenue)            # Reordena de forma ascendente para graficar de abajo hacia arriba

# Crear el gráfico de barras horizontal
ggplot(movies_top10, aes(x = revenue, y = reorder(title, revenue))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 películas por ingresos",
       x = "Ingresos",
       y = "Película") +
  theme_minimal()

## 4.4. (3 puntos) ¿Cuál es la peor película de acuerdo a los votos de todos los usuarios?

peor_pelicula <- movies_cleaned %>%
  arrange(voteAvg) %>%  # Ordena de menor a mayor según voteAvg
  head(1)               # Selecciona la película con la calificación más baja

# Mostrar la peor película
peor_pelicula

# Calcular la calificación promedio de todas las películas
calificacion_media <- mean(movies_cleaned$voteAvg, na.rm = TRUE)

# Crear un data frame para la visualización
df_votos <- data.frame(
  Categoria = c("Peor Película", "Promedio del Conjunto"),
  Calificacion = c(peor_pelicula$voteAvg, calificacion_media)
)
# Usando drop_na() para eliminar NAs en la columna 'genre1'
genre_counts <- movies_recent_20 %>%
  drop_na(genre1) %>%         # Elimina filas con NA en genre1
  count(genre1, sort = TRUE)

# Generar el gráfico de barras
ggplot(df_votos, aes(x = Categoria, y = Calificacion, fill = Categoria)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de Calificaciones: Peor Película vs Promedio",
       x = "Categoría",
       y = "Calificación Promedio") +
  theme_minimal() +
  scale_fill_manual(values = c("Peor Película" = "tomato", "Promedio del Conjunto" = "steelblue"))

## 4.6. (9 puntos) ¿Cuál es el género principal de las 20 películas más recientes? ¿Cuál es el género

# Filtrar las 20 películas más recientes excluyendo aquellos casos donde genre1 es NA
movies_recent_20 <- movies_cleaned %>%
  filter(!is.na(genre1)) %>%           # Excluir filas con NA en genre1
  arrange(desc(releaseDate)) %>%       # Ordenar de más reciente a más antiguo según releaseDate
  head(20)                             # Seleccionar las 20 películas más recientes

# Contar la frecuencia de cada género principal
genre_counts <- movies_recent_20 %>%
  count(genre1, sort = TRUE)

# Imprimir la tabla de frecuencias
print(genre_counts)

# Crear un gráfico de barras horizontal para visualizar la distribución de géneros
ggplot(genre_counts, aes(x = reorder(genre1, n), y = n, fill = genre1)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución del género principal en las 20 películas más recientes",
       x = "Género principal",
       y = "Cantidad de películas") +
  coord_flip() +
  theme_minimal()

# Cargar las librerías necesarias
library(tidyverse)

# Leer los datos desde el archivo CSV
movies_cleaned <- read_csv("data/movies_cleaned.csv")

## Análisis 1: Género principal de las 20 películas más recientes (excluyendo NAs en genre1)
movies_recent_20 <- movies_cleaned %>%
  filter(!is.na(genre1)) %>%         # Excluir filas con NA en genre1
  arrange(desc(releaseDate)) %>%     # Ordenar de más reciente a más antiguo
  head(20)                           # Seleccionar las 20 películas más recientes

# Contar la frecuencia de cada género principal en este subconjunto
genre_counts_recent <- movies_recent_20 %>%
  count(genre1, sort = TRUE)

# Mostrar la tabla de frecuencias
print(genre_counts_recent)

# Gráfico de barras horizontal para visualizar la distribución de géneros en las 20 películas más recientes
ggplot(genre_counts_recent, aes(x = reorder(genre1, n), y = n, fill = genre1)) +
  geom_bar(stat = "identity") +
  labs(title = "Género principal en las 20 películas más recientes",
       x = "Género principal",
       y = "Cantidad de películas") +
  coord_flip() +
  theme_minimal()


## Análisis 2: ¿A qué género principal pertenecen las películas más largas?
# Filtrar las 10 películas con mayor duración, excluyendo NAs en runtime y genre1
long_movies <- movies_cleaned %>%
  filter(!is.na(runtime), !is.na(genre1)) %>%
  arrange(desc(runtime)) %>%
  head(10)

# Contar la frecuencia del género principal en estas películas
genre_counts_long <- long_movies %>%
  count(genre1, sort = TRUE)

# Mostrar la tabla de frecuencias
print(genre_counts_long)

# Gráfico de barras horizontal para visualizar el género principal de las películas más largas
ggplot(genre_counts_long, aes(x = reorder(genre1, n), y = n, fill = genre1)) +
  geom_bar(stat = "identity") +
  labs(title = "Género principal de las 10 películas más largas",
       x = "Género principal",
       y = "Cantidad de películas") +
  coord_flip() +
  theme_minimal()
# Cargar las librerías necesarias
library(tidyverse)
library(lubridate)

# Asegurarse de que los datos estén cargados
movies_cleaned <- read_csv("data/movies_cleaned.csv")
## 4.8. (3 puntos) ¿La cantidad de actores influye en los ingresos de las películas?¿se han hecho
películas con más actores en los últimos años?
  
  
## Análisis 1: ¿La cantidad de actores influye en los ingresos de las películas?
# Filtrar filas sin NA en actorsAmount y revenue
movies_cleaned_filtered <- movies_cleaned %>%
  filter(!is.na(actorsAmount), !is.na(revenue))

# Crear un scatter plot para visualizar la relación
ggplot(movies_cleaned_filtered, aes(x = actorsAmount, y = revenue)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "tomato") +
  labs(title = "Relación entre cantidad de actores e ingresos",
       x = "Cantidad de actores",
       y = "Ingresos") +
  theme_minimal()

## Análisis 2: ¿Se han hecho películas con más actores en los últimos años?
# Filtrar filas sin NA en actorsAmount y releaseDate, y extraer el año
movies_cleaned_filtered2 <- movies_cleaned %>%
  filter(!is.na(actorsAmount), !is.na(releaseDate)) %>%
  mutate(year = year(releaseDate))

# Calcular el promedio de actores por año
avg_actors_by_year <- movies_cleaned_filtered2 %>%
  group_by(year) %>%
  summarise(avg_actors = mean(actorsAmount))

# Crear un gráfico de línea para visualizar la tendencia a lo largo del tiempo
ggplot(avg_actors_by_year, aes(x = year, y = avg_actors)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "tomato", size = 2) +
  labs(title = "Tendencia de la cantidad de actores en películas",
       x = "Año",
       y = "Promedio de actores") +
  theme_minimal()


summary(data_filtered$actorsAmount)


ggplot(data_filtered, aes(x = actorsAmount)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, max(data_filtered$actorsAmount), by = 20)) +
  labs(title = "Distribución del número de actores por película",
       x = "Número de actores",
       y = "Frecuencia")

# Contar películas con más de 100 actores
data_filtered %>%
  summarize(
    total_movies = n(),
    movies_over_100 = sum(actorsAmount > 100),
    percentage = (movies_over_100 / total_movies) * 100
  )
# Contar películas con más de 100 actores
data_filtered %>%
  summarize(
    total_movies = n(),
    movies_over_100 = sum(actorsAmount > 100),
    percentage = (movies_over_100 / total_movies) * 100
  )


# Contar películas con más de 100 actores
data_filtered %>%
  summarize(
    total_movies = n(),
    movies_over_100 = sum(actorsAmount > 100),
    percentage = (movies_over_100 / total_movies) * 100
  )

table(data_filtered$revenue)
