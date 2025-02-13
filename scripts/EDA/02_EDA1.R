# 1. Cargar las bibliotecas necesarias
library(tidyverse)

# 2. Leer el conjunto de datos limpio
movies_cleaned <- read_csv("data/movies_cleaned.csv")

# 3. Exploración básica de los datos
View(movies_cleaned)          # Visualizar el dataset en una ventana
nrow(movies_cleaned)          # Cantidad de filas
ncol(movies_cleaned)          # Cantidad de columnas
str(movies_cleaned)           # Estructura de los datos

# Mostrar las primeras y últimas filas
head(movies_cleaned, 3)       # Primeras 3 filas
tail(movies_cleaned, 3)       # Últimas 3 filas

# Mostrar las primeras 3 filas de las primeras 5 columnas
head(movies_cleaned[, 1:5], 3)
# Crear un dataframe con los tipos de variables
variable_types <- tibble(
  Variable = names(movies_cleaned),
  Tipo = c(
    "Cuantitativa Discreta",     # id
    "Cuantitativa Continua",     # budget
    "Cualitativa Nominal",       # genre1
    "Cualitativa Nominal",       # genre2
    "Cualitativa Nominal",       # genre3
    "Cualitativa Nominal",       # homePage
    "Cualitativa Nominal",       # productionCompany
    "Cualitativa Nominal",       # productionCompanyCountry
    "Cualitativa Nominal",       # productionCountry
    "Cuantitativa Continua",     # revenue
    "Cuantitativa Continua",     # runtime
    "Cualitativa Nominal",       # video
    "Cualitativa Nominal",       # director
    "Cualitativa Nominal",       # actors
    "Cuantitativa Continua",     # actorPopularity1
    "Cuantitativa Continua",     # actorPopularity2
    "Cuantitativa Continua",     # actorPopularity3
    "Cuantitativa Continua",     # actorPopularity4
    "Cuantitativa Continua",     # actorPopularity5
    "Cualitativa Nominal",       # actorsCharacter
    "Cualitativa Nominal",       # originalTitle
    "Cualitativa Nominal",       # title
    "Cualitativa Nominal",       # originalLanguage
    "Cuantitativa Continua",     # popularity
    "Fecha",                     # releaseDate
    "Cuantitativa Continua",     # voteAvg
    "Cuantitativa Discreta",     # voteCount
    "Cuantitativa Discreta",     # genresAmount
    "Cuantitativa Discreta",     # productionCoAmount
    "Cuantitativa Discreta",     # productionCountriesAmount
    "Cuantitativa Discreta",     # actorsAmount
    "Cuantitativa Discreta",     # castWomenAmount
    "Cuantitativa Discreta"      # castMenAmount
  )
)

# Mostrar la clasificación
print(variable_types)

movies_df %>% print(n = 50)

# Pregunta 3 
# Seleccionar variables cuantitativas
quantitative_vars <- movies_cleaned %>%
  select(
    budget, revenue, runtime, popularity, voteAvg, voteCount,
    genresAmount, productionCoAmount, productionCountriesAmount,
    actorsAmount, castWomenAmount, castMenAmount
  )

# Resumen estadístico
summary(quantitative_vars)

# Desviación estándar para cada variable
sd_budget <- sd(movies_cleaned$budget, na.rm = TRUE)
sd_revenue <- sd(movies_cleaned$revenue, na.rm = TRUE)
sd_runtime <- sd(movies_cleaned$runtime, na.rm = TRUE)
sd_popularity <- sd(movies_cleaned$popularity, na.rm = TRUE)
sd_voteAvg <- sd(movies_cleaned$voteAvg, na.rm = TRUE)
sd_voteCount <- sd(movies_cleaned$voteCount, na.rm = TRUE)
sd_genresAmount <- sd(movies_cleaned$genresAmount, na.rm = TRUE)
sd_productionCoAmount <- sd(movies_cleaned$productionCoAmount, na.rm = TRUE)
sd_productionCountriesAmount <- sd(movies_cleaned$productionCountriesAmount, na.rm = TRUE)
sd_actorsAmount <- sd(movies_cleaned$actorsAmount, na.rm = TRUE)
sd_castWomenAmount <- sd(movies_cleaned$castWomenAmount, na.rm = TRUE)
sd_castMenAmount <- sd(movies_cleaned$castMenAmount, na.rm = TRUE)

# Imprimir desviaciones estándar
cat("Desviación Estándar del Presupuesto:", round(sd_budget, 2), "\n")
cat("Desviación Estándar de los Ingresos:", round(sd_revenue, 2), "\n")
cat("Desviación Estándar de la Duración:", round(sd_runtime, 2), "\n")
cat("Desviación Estándar de la Popularidad:", round(sd_popularity, 2), "\n")
cat("Desviación Estándar de la Calificación Promedio:", round(sd_voteAvg, 2), "\n")
cat("Desviación Estándar del Número de Votos:", round(sd_voteCount, 2), "\n")
cat("Desviación Estándar de la Cantidad de Géneros:", round(sd_genresAmount, 2), "\n")
cat("Desviación Estándar de la Cantidad de Compañías Productoras:", round(sd_productionCoAmount, 2), "\n")
cat("Desviación Estándar de la Cantidad de Países de Producción:", round(sd_productionCountriesAmount, 2), "\n")
cat("Desviación Estándar de la Cantidad de Actores:", round(sd_actorsAmount, 2), "\n")
cat("Desviación Estándar de la Cantidad de Actrices:", round(sd_castWomenAmount, 2), "\n")
cat("Desviación Estándar de la Cantidad de Actores Masculinos:", round(sd_castMenAmount, 2), "\n")
