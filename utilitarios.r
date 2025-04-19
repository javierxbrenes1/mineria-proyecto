# Load required libraries
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(tidyr) 
library(corrplot)
library(knitr)
library(kableExtra)
library(factoextra)
library(NbClust)
library(caret)
library(forecast)
library(lubridate)


limpiar_texto <- function(texto) {
  texto <- tolower(texto)
  texto <- stri_trans_general(texto, "Latin-ASCII")
  texto <- str_trim(texto)
  return(texto)
}


circuitos_provincias <- data.frame(
    circuito = c(
        'San Jose',
        'Heredia',
        'Alajuela',
        'Cartago',
        'Guanacaste',
        'Puntarenas',
        'Limon',
        'ATLÁNTICA',
        'Sur',
        'ATENAS',
        'Turrialba',
        'Golfito',
        "PEREZ ZELEDON"
        
    ),
    provincia = c(
        'San Jose',
        'Heredia',
        'Alajuela',
        'Cartago',
        'Guanacaste',
        'Puntarenas',
        'Limon',
        'Limon',
        'Puntarenas',
        'Alajuela',
        'Cartago',
        'Puntarenas',
        "San Jose"
    )
)

circuitos_provincias

# Function to load domestic violence data
cargar_datos_violencia <- function(ruta = "data_sources/ARCHIVO_VIOLENCIA_DOMESTICA.tsv") {
  # Validate if file exists
  if (!file.exists(ruta)) {
    stop("Error: El archivo no existe en la ruta especificada: ", ruta)
  }
  

    # Read CSV file
    datos <- read.csv(ruta, 
                     stringsAsFactors = FALSE, 
                     encoding = "UTF-8",
                     check.names = FALSE,
                     sep = '\t')

    return(datos)
}


clasificar_circuito <- function(circuitos) {
    resultados <- rep(NA_character_, length(circuitos))
    
    for(i in seq_len(nrow(circuitos_provincias))) {
        matches <- str_detect(limpiar_texto(circuitos), limpiar_texto(circuitos_provincias$circuito[i]))
        resultados[matches] <- circuitos_provincias$provincia[i]
    }
    return(resultados)
}

calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")