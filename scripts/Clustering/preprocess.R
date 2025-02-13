# Cargar librerías necesarias
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

# 5. Funciones de limpieza
clean_numeric <- function(x) {
  if(is.numeric(x)) return(x)
  cleaned <- str_replace_all(x, "[^0-9\\.\\-]", "")
  cleaned[cleaned == ""] <- NA
  as.numeric(cleaned)
}

clean_actorsPopularity <- function(x) {
  parts <- unlist(strsplit(x, "\\|"))
  parts <- str_trim(parts)
  parts <- str_replace_all(parts, "[^0-9\\.\\-]", "")
  parts[parts == ""] <- NA
  nums <- as.numeric(parts)
  if(all(is.na(nums))) return(NA)
  mean(nums, na.rm = TRUE)
}

# 6. Aplicar limpieza a columnas problemáticas
movies_clean <- movies_clean %>% 
  mutate(
    castWomenAmount = clean_numeric(castWomenAmount),
    castMenAmount   = clean_numeric(castMenAmount),
    actorsPopularity = sapply(as.character(actorsPopularity), clean_actorsPopularity)
  )


cat("Valores faltantes después de la limpieza:\n")
print(colSums(movies_clean[sapply(movies_clean, is.numeric)]))

# 7. Imputación de NA en variables numéricas usando la mediana
num_vars <- c("popularity", "budget", "revenue", "runtime", "genresAmount",
              "productionCoAmount", "productionCountriesAmount", "voteCount",
              "voteAvg", "actorsPopularity", "actorsAmount", "castWomenAmount",
              "castMenAmount", "releaseYear")

movies_clean <- movies_clean %>% 
  mutate(across(all_of(num_vars), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# 8. Eliminar filas que aún tengan NA (por seguridad)
movies_clean <- na.omit(movies_clean)

# 9. Escalado de variables numéricas (normalización Z-score)
movies_clean[num_vars] <- scale(movies_clean[num_vars])

# 10. Descartar las variables que no aportan discriminación
movies_clean <- movies_clean %>% select(-castWomenAmount, -castMenAmount)

# 11. Guardar el dataset preprocesado
write.csv(movies_clean, "./data/movies_clean_scaled.csv", row.names = FALSE)

# Verificar la estructura final
movies_clean_scaled <- read.csv("./data/movies_clean_scaled.csv", stringsAsFactors = FALSE)
cat("Estructura del dataset preprocesado:\n")
str(movies_clean_scaled)
cat("Resumen del dataset preprocesado:\n")
summary(movies_clean_scaled)
