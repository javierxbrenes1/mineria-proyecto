source("utilitarios.r")
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(tidyr)  # For pivot_longer
options(width = 2000) 
# Cargar datos de violencia doméstica
datos_violencia <- cargar_datos_violencia()

# Function to calculate mode
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

rr <- datos_violencia %>% 
  select(where(is.numeric), -Anno, -Mes) 
summary(rr)

rr %>% filter(is.na(CirculanteFinalLegajos))
