###############################################################################
# PHASE 2: Data Import & Cleaning (Adjusted for CineVision Studios)
###############################################################################

# 1. Load Required Libraries
library(tidyverse)
library(lubridate)
library(stringr)

# 2. Define Logging Function
log_change <- function(message) {
  cat(paste0("[", Sys.time(), "] ", message, "\n"))
}

###############################################################################
# 3. Read the Raw Data with Basic Error Handling
###############################################################################
log_change("Attempting to read the CSV file: data/movies.csv")

movies_df <- tryCatch(
  {
    read_csv(
      file = "data/movies.csv",
      locale = locale(encoding = "UTF-8"),
      show_col_types = FALSE
    )
  },
  error = function(e) {
    message("Error reading the CSV file: ", e$message)
    NULL
  }
)

if (is.null(movies_df)) {
  stop("Data import failed. Please check the CSV file and try again.")
}

log_change("CSV file successfully read.")

###############################################################################
# 4. Basic Checks & Exploration
###############################################################################
log_change("Performing initial data checks.")
print(dim(movies_df))
print(glimpse(movies_df))
print(summary(movies_df))

###############################################################################
# 5. Handle Duplicates
###############################################################################
log_change("Checking for and removing duplicate rows.")
before_duplicates <- nrow(movies_df)
movies_df <- distinct(movies_df)
after_duplicates <- nrow(movies_df)
duplicates_removed <- before_duplicates - after_duplicates
log_change(paste("Removed", duplicates_removed, "duplicate rows."))

###############################################################################
# 6. Handle Missing Values
###############################################################################
log_change("Handling missing values.")

# Reemplazar NAs en 'director' y 'productionCountry'
movies_df <- movies_df %>%
  mutate(
    director = replace_na(director, "Desconocido"),
    productionCountry = replace_na(productionCountry, "Desconocido")
  )

# Reemplazar NAs en 'actorsPopularity' con la mediana
all_actors_popularity <- movies_df %>%
  pull(actorsPopularity) %>%
  str_split("\\|") %>%
  unlist() %>%
  as.numeric()

median_actors_popularity <- median(all_actors_popularity, na.rm = TRUE)

# Limpieza y conversión de 'castWomenAmount' y 'castMenAmount'
log_change("Cleaning and converting 'castWomenAmount' and 'castMenAmount'.")

movies_df <- movies_df %>%
  mutate(
    # Eliminar caracteres no numéricos
    castWomenAmount = str_replace_all(castWomenAmount, "[^0-9]", ""),
    castMenAmount = str_replace_all(castMenAmount, "[^0-9]", "")
  ) %>%
  mutate(
    # Convertir a numérico
    castWomenAmount = as.numeric(castWomenAmount),
    castMenAmount = as.numeric(castMenAmount)
  ) %>%
  mutate(
    # Reemplazar valores sospechosos con NA
    castWomenAmount = if_else(castWomenAmount > 500 | castWomenAmount < 0, NA_real_, castWomenAmount),
    castMenAmount = if_else(castMenAmount > 500 | castMenAmount < 0, NA_real_, castMenAmount)
  )

# Reemplazar NAs en 'castWomenAmount' y 'castMenAmount' con 0 si es apropiado
movies_df <- movies_df %>%
  mutate(
    castWomenAmount = replace_na(castWomenAmount, 0),
    castMenAmount = replace_na(castMenAmount, 0)
  )

log_change("Missing values handled.")

###############################################################################
# 7. Convert & Clean Problematic Columns
###############################################################################
log_change("Converting and cleaning problematic columns.")

movies_df <- movies_df %>%
  mutate(
    # Convertir 'releaseDate' a formato Date
    releaseDate = as.Date(releaseDate, format = "%Y-%m-%d"),
    
    # Manejar fechas inválidas
    releaseDate = if_else(year(releaseDate) < 1900 | year(releaseDate) > year(Sys.Date()), NA_Date_, releaseDate),
    
    # Convertir 'video' a lógico, manejar NAs
    video = replace_na(video, FALSE),
    
    # Reemplazar valores extremos en 'budget'
    budget = if_else(budget > 500000000 | budget <= 0, NA_real_, budget),
    
    # Reemplazar valores extremos en 'revenue'
    revenue = if_else(revenue < 0 | revenue > 1e10, NA_real_, revenue),
    
    # Reemplazar valores extremos en 'runtime'
    runtime = if_else(runtime > 400 | runtime <= 0, NA_real_, runtime)
  ) %>%
  # Manejar 'genres' y 'actorsPopularity'
  separate(genres, into = c("genre1", "genre2", "genre3", "genre4", "genre5"), 
           sep = "\\|", extra = "drop", fill = "right") %>%
  mutate(
    actorsPopularity = str_replace_all(actorsPopularity, "[^0-9\\.\\|]", "")
  ) %>%
  separate(
    actorsPopularity,
    into = paste0("actorPopularity", 1:10),  # Aumentar el número de columnas si es necesario
    sep = "\\|",
    extra = "drop",
    fill = "right"
  ) %>%
  mutate(across(starts_with("actorPopularity"), as.numeric)) %>%
  mutate(across(starts_with("actorPopularity"), ~replace_na(., median(., na.rm = TRUE)))) %>%
  # Eliminar caracteres no imprimibles de columnas de texto
  mutate(across(where(is.character), ~ str_replace_all(., "[^[:print:]]", ""))) %>%
  # Filtrar outliers adicionales en 'voteAvg'
  mutate(
    voteAvg = if_else(voteAvg < 0 | voteAvg > 10, NA_real_, voteAvg)
  ) %>%
  # Reemplazar NAs en 'voteAvg'
  mutate(
    voteAvg = replace_na(voteAvg, median(voteAvg, na.rm = TRUE))
  )

log_change("Conversion and cleaning of problematic columns completed.")

###############################################################################
# 8. Handle Outliers and Additional Cleaning
###############################################################################
log_change("Handling outliers and performing additional cleaning.")

movies_df <- movies_df %>%
  mutate(
    # Reemplazar 'popularity' extremos
    popularity = if_else(popularity < 0 | popularity > 1000, NA_real_, popularity),
    popularity = replace_na(popularity, median(popularity, na.rm = TRUE))
  ) %>%
  mutate(
    # Reemplazar 'voteCount' extremos
    voteCount = if_else(voteCount < 0 | voteCount > 1e6, NA_real_, voteCount),
    voteCount = replace_na(voteCount, median(voteCount, na.rm = TRUE))
  )

log_change("Outliers handled.")

###############################################################################
# 9. Review and Fix 'Frecuencia' Error
###############################################################################
movies_df <- movies_df %>%
  mutate(
    # Contar la cantidad de géneros no NA (lo que se pretendía con Frecuencia)
    genresCount = rowSums(!is.na(select(., genre1:genre5)))
  )

log_change("Reviewed and fixed 'Frecuencia' related errors.")

###############################################################################
# 10. Additional Checks & Export Cleaned Data
###############################################################################
log_change("Performing post-cleaning data checks.")
print(summary(movies_df))

# Verificar la cantidad de NAs por columna
na_counts <- sapply(movies_df, function(x) sum(is.na(x)))
log_change("NA counts per column after cleaning:")
print(na_counts)

# Fix file writing error - use try-catch and ensure directory exists
tryCatch({
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  write.csv(movies_df, "data/movies_cleaned.csv", row.names = FALSE)
  message("Successfully saved cleaned data")
}, error = function(e) {
  alt_path <- file.path(getwd(), "movies_cleaned.csv")
  write.csv(movies_df, alt_path, row.names = FALSE)
  message("Saved data to current working directory: ", alt_path)
})

###############################################################################
# 12. Final Logging and Cleanup
###############################################################################
log_change("All tasks completed successfully.")

