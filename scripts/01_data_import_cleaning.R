###############################################################################
# PHASE 2: Data Import & Cleaning (Adjusted for CineVision Studios)
###############################################################################

# 1. Load Required Libraries
library(tidyverse)
library(lubridate)
library(stringr)

# 2. Read the Raw Data with Basic Error Handling
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

###############################################################################
# 3. Basic Checks & Exploration
###############################################################################
message("Initial Data Checks:")
print(dim(movies_df))
print(glimpse(movies_df))
print(summary(movies_df))

###############################################################################
# 4. Handle Duplicates
###############################################################################
movies_df <- distinct(movies_df)

###############################################################################
# 5. Handle Missing Values
###############################################################################
message("Handling Missing Values:")
movies_df <- movies_df %>%
    mutate(
        director = replace_na(director, "Desconocido"),
        productionCountry = replace_na(productionCountry, "Desconocido"),
        actorsPopularity = replace_na(actorsPopularity, median(actorsPopularity, na.rm = TRUE)),
        castWomenAmount = as.numeric(str_replace_all(castWomenAmount, "[^0-9]", "")),
        castMenAmount = as.numeric(str_replace_all(castMenAmount, "[^0-9]", ""))
    ) %>%
    mutate(
        castWomenAmount = replace_na(castWomenAmount, 0),
        castMenAmount = replace_na(castMenAmount, 0)
    )

###############################################################################
# 6. Convert & Clean Problematic Columns
###############################################################################
movies_df <- movies_df %>%
    mutate(
        releaseDate = as.Date(releaseDate, format = "%Y-%m-%d"),
        video = as.logical(video),
        budget = if_else(budget > 500000000, NA_real_, budget),
        revenue = if_else(revenue < 0, NA_real_, revenue),
        runtime = if_else(runtime > 400, NA_real_, runtime)
    )

# Handle genres and actorsPopularity
movies_df <- movies_df %>%
    separate(genres, into = c("genre1", "genre2", "genre3"), sep = "\\|", extra = "merge", fill = "right") %>%
    mutate(
        actorsPopularity = str_replace_all(actorsPopularity, "[^0-9\\|\\.]", "")
    ) %>%
    separate(
        actorsPopularity,
        into = c("actorPopularity1", "actorPopularity2", "actorPopularity3", "actorPopularity4", "actorPopularity5"),
        sep = "\\|",
        extra = "drop",
        fill = "right"
    ) %>%
    mutate(across(starts_with("actorPopularity"), as.numeric)) %>%
    mutate(across(starts_with("actorPopularity"), ~replace_na(., median(., na.rm = TRUE))))

# Remove non-printable characters from character columns
movies_df <- movies_df %>%
    mutate(across(where(is.character), ~ str_replace_all(., "[^[:print:]]", "")))

# Filter out extreme values
movies_df <- movies_df %>% filter(runtime <= 400 | is.na(runtime))

###############################################################################
# 7. Additional Checks & Export Cleaned Data
###############################################################################
message("Post-Cleaning Data Checks:")
print(summary(movies_df))

# Save cleaned dataset
write_csv(movies_df, "data/movies_cleaned.csv")
message("Data cleaning complete. 'movies_cleaned.csv' has been saved.")
