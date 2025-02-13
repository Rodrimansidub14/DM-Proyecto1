library(tidyverse)
library(lubridate)
library(stringr)

# 1. Leer el dataset y explorarlo
movies <- read.csv("./data/movies.csv", stringsAsFactors = FALSE)
cat("Estructura del dataset original:\n")
str(movies)
cat("Resumen del dataset original:\n")
summary(movies)
cat("Valores faltantes por columna:\n")
print(colSums(is.na(movies)))

# 2. Selección de variables irrelevantes
movies_clean <- movies %>% 
  select(-id, -originalTitle, -title, -homePage, -productionCompany, -genres, -director, -actors, -actorsCharacter)

# 3. Transformación de la fecha: convertir 'releaseDate' a Date y extraer el año
movies_clean <- movies_clean %>% 
  mutate(releaseDate = as.Date(releaseDate, format = "%Y-%m-%d"),
         releaseYear = year(releaseDate)) %>% 
  select(-releaseDate)

# 4. Conversión a variables categóricas
movies_clean <- movies_clean %>% 
  mutate(
    originalLanguage = as.factor(originalLanguage),
    video = as.factor(video),
    productionCompanyCountry = as.factor(productionCompanyCountry),
    productionCountry = as.factor(productionCountry)
  )

# 5. Limpieza de columnas numéricas problemáticas
# Función para limpiar y convertir a numérico
clean_numeric <- function(x) {
  cleaned <- str_replace_all(x, "[^0-9\\.\\-]", "")  # Elimina todo salvo dígitos, punto y signo menos
  cleaned[cleaned == ""] <- NA                        # Si quedó cadena vacía, se asigna NA
  as.numeric(cleaned)
}

# Función para procesar actorsPopularity
clean_actorsPopularity <- function(x) {
  parts <- unlist(strsplit(x, "\\|"))
  parts <- str_trim(parts)
  parts <- str_replace_all(parts, "[^0-9\\.\\-]", "")
  parts[parts == ""] <- NA
  nums <- as.numeric(parts)
  nums <- nums[!is.na(nums)]
  if(length(nums) == 0) return(NA) else return(mean(nums))
}

# Supongamos que ya tenemos movies_clean tras la selección de variables y transformación de fechas:
movies_clean <- movies %>% 
  select(-id, -originalTitle, -title, -homePage, -productionCompany, -genres, -director, -actors, -actorsCharacter) %>% 
  mutate(releaseDate = as.Date(releaseDate, format = "%Y-%m-%d"),
         releaseYear = year(releaseDate)) %>% 
  select(-releaseDate) %>% 
  mutate(
    originalLanguage = as.factor(originalLanguage),
    video = as.factor(video),
    productionCompanyCountry = as.factor(productionCompanyCountry),
    productionCountry = as.factor(productionCountry)
  )

# Aplicar la limpieza :
movies_clean <- movies_clean %>% 
  mutate(
    castWomenAmount = clean_numeric(castWomenAmount),
    castMenAmount   = clean_numeric(castMenAmount),
    actorsPopularity = sapply(actorsPopularity, clean_actorsPopularity)
  )

# Verificamos los NA resultantes
cat("Valores faltantes después de la limpieza:\n")
print(colSums(is.na(movies_clean)))
# Limpiar 'castWomenAmount' y 'castMenAmount': quitar espacios y convertir a numérico
movies_clean <- movies_clean %>% 
  mutate(
    castWomenAmount = as.numeric(trimws(castWomenAmount)),
    castMenAmount   = as.numeric(trimws(castMenAmount))
  )
sapply(movies_clean[c("castWomenAmount", "castMenAmount")], class)

# Procesar 'actorsPopularity': convertir cadena separada por "|" en promedio numérico
movies_clean <- movies_clean %>% 
  mutate(actorsPopularity = sapply(as.character(actorsPopularity), function(x) {
    nums <- as.numeric(unlist(strsplit(x, "\\|")))
    mean(nums, na.rm = TRUE)
  }))


cat("Valores faltantes después de la limpieza:\n")
print(colSums(is.na(movies_clean)))

# 6. Imputación de NA en variables numéricas usando la mediana
num_vars <- c("popularity", "budget", "revenue", "runtime", "genresAmount",
              "productionCoAmount", "productionCountriesAmount", "voteCount",
              "voteAvg", "actorsPopularity", "actorsAmount", "castWomenAmount",
              "castMenAmount", "releaseYear")

movies_clean <- movies_clean %>% 
  mutate(across(all_of(num_vars), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Eliminar filas que aún tengan NA (por seguridad)
movies_clean <- na.omit(movies_clean)

# 7. Escalado de variables numéricas (normalización Z-score)
movies_clean[num_vars] <- scale(movies_clean[num_vars])

# 8. Guardar el dataset preprocesado
write.csv(movies_clean, "./data/movies_clean_scaled.csv", row.names = FALSE)

movies_clean_scaled <- read.csv("./data/movies_clean_scaled.csv")
cat("Estructura del dataset original:\n")
str(movies_clean_scaled)
cat("Resumen del dataset original:\n")
summary(movies_clean_scaled)