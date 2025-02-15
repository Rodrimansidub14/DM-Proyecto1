# Carga de librerías
library(psych)
library(FactoMineR)
library(corrplot)
library(fpc)
library(car)

library(factoextra)
library(PCAmixdata)
library(paran)
library(graph)
library(ggplot2)
library(reshape2)
library(corrplot)

# 1. Cargar el dataset preprocesado
# (Ajuste la ruta según la ubicación del archivo)
movies_clean_transformed <- read.csv("C:/Users/rodri/Documents/Data-Mining/Proyecto 1/data/movies_clean_transformed.csv", 
                                     stringsAsFactors = FALSE)
# 2. Verificar la estructura y resumen del dataset
str(movies_clean_transformed)
cat("Resumen del dataset preprocesado:\n")
summary(movies_clean_transformed)

# 3. Revisar la existencia de valores faltantes en todas las variables
na_por_variable <- colSums(is.na(movies_clean_transformed))
print(na_por_variable)

# 4. Separar variables numéricas y categóricas
# Variables numéricas (según el resumen, estas son las que se han escalado y transformado)
vars_numericas <- c("budget", "revenue", "runtime", "actorsPopularity", 
                    "popularity", "voteAvg", "voteCount", "genresAmount", 
                    "productionCoAmount", "productionCountriesAmount", 
                    "actorsAmount", "releaseYear")
movies_numeric <- movies_clean_scaled[, vars_numericas]

# Variables categóricas y lógicas que podrían considerarse para otros análisis o transformación
vars_categoricas <- c("productionCompanyCountry", "productionCountry", "originalLanguage", "video")
movies_categoricas <- movies_clean_scaled[, vars_categoricas]

# 5. Exploración básica de las variables categóricas
# Ejemplo: conteo de niveles para 'video' y 'originalLanguage'
table(movies_categoricas$video)
table(movies_categoricas$originalLanguage)

# 6. Visualización preliminar: Distribución de las variables numéricas

# Convertir los datos numéricos a formato largo para graficar
melted_data <- melt(movies_numeric)
ggplot(melted_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "#00AFBB", color = "black") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribución de Variables Numéricas", x = "Valor", y = "Frecuencia")



##Seleccionar variables numéricas para análisis gráfico
vars_numericas <- c("popularity_log", "budget_log", "revenue_log", "runtime", 
                    "genresAmount", "productionCoAmount", "productionCountriesAmount",
                    "voteCount_log", "voteAvg", "actorsPopularity", "actorsAmount", "releaseYear")
movies_numeric <- movies_clean_transformed[, vars_numericas]

# Histogramas: Distribución de cada variable numérica
melted_data <- melt(movies_numeric)
ggplot(melted_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "#00AFBB", color = "black") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribución de Variables Numéricas", x = "Valor", y = "Frecuencia")

# Boxplots: Identificación de outliers en las variables numéricas
ggplot(melted_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "#E7B800") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot de Variables Numéricas", x = "Variable", y = "Valor")

# Matriz de correlación entre variables numéricas (útil para PCA y clustering)
cor_matrix <- cor(movies_numeric)
corrplot(cor_matrix, method = "circle", tl.cex = 0.8)

### fACTIBILIDAD
# Seleccionamos las variables numéricas transformadas
vars_numericas <- c("popularity_log", "budget_log", "revenue_log", "runtime", 
                    "genresAmount", "productionCoAmount", "productionCountriesAmount",
                    "voteCount_log", "voteAvg", "actorsPopularity", "actorsAmount", "releaseYear")
movies_numeric <- movies_clean_transformed[, vars_numericas]

# 1. Calcular la matriz de correlación y su determinante
cor_matrix <- cor(movies_numeric, use = "pairwise.complete.obs")
print(cor_matrix)
det_val <- det(cor_matrix)
cat("Determinante de la matriz de correlación:", det_val, "\n")

# 2. Índice KMO
kmo_result <- KMO(movies_numeric)
print(kmo_result)

# 3. Test de esfericidad de Bartlett
bartlett_result <- cortest.bartlett(movies_numeric)
print(bartlett_result)


# 4. Análisis de Componentes Principales (PCA)

compPrincipal <- prcomp(movies_numeric, scale = TRUE)
compPrincipal 

summary(compPrincipal) 


## Regla de Kaiser

valores_propios <- compPrincipal$sdev^2
valores_propios



### regla de sedimentación
fviz_eig(compPrincipal, addlabels = TRUE, ylim = c(0, 80))

fviz_eig(compPrincipal, addlabels = TRUE, ylim = c(0, 4),choic = "eigenvalue")


summary(compPrincipal)



## Paralelismo

# Ejecutar el análisis paralelo sin argumento main
# Suponiendo que 'movies_numeric' es tu data frame con las variables numéricas seleccionadas:
paran_result <- paran(movies_numeric, graph = TRUE)
paran_result <- paran(movies_numeric{,-1}, graph = TRUE)
# Agregar título a la gráfica
title("Análisis de Paralelismo")


## Carga Factorial

if(!require(factoextra)) install.packages("factoextra")
library(factoextra)

fviz_pca_var(compPrincipal, col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)


## contribución de las variables en las primeras dimensiones

fviz_contrib(compPrincipal, choice = "var", axes = 1, top = 10)

fviz_contrib(compPrincipal, choice = "var", axes = 2, top = 10)

fviz_contrib(compPrincipal, choice = "var", axes = 3, top = 10)

fviz_contrib(compPrincipal, choice = "var", axes = 4, top = 10)
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)

corrplot(var$cos2, is.corr = FALSE)


var <- get_pca_var(compPrincipal)
corrplot(var$cos2, is.corr = FALSE)
