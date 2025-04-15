# 1. Distribución Geográfica de la Violencia Doméstica:
# Pregunta de Investigación: 
# ¿Existe una variación significativa en la incidencia de casos de violencia doméstica entre áreas del gran área metropolitana y áreas fuera del área metropolitana?

# Objetivos:
# Analizar la distribución geográfica de los casos nuevos y terminados de violencia doméstica por circuito judicial.
# Realizar una comparación de la incidencia de casos entre áreas del gran área metropolitana y áreas fuera del área metropolitana

# Requerimientos de Datos:
# Depuración y homogeneización del conjunto de datos existente.
# Enriquecimiento del conjunto de datos para permitir la clasificación de los circuitos judiciales en áreas del gran área metropolitana y áreas fuera del área metropolitana.

# Importar funciones utilitarias
source("utilitarios.r")
library(dplyr)
library(stringr)
library(stringi)

options(width = 2000) 
# Cargar datos de violencia doméstica
datos_violencia <- cargar_datos_violencia()

# Verificar la estructura de los datos
str(datos_violencia)
head(datos_violencia, 1)
datos_violencia$NombreCircuito

# agregar la provincia como parte del dataSet
datos_violencia <- datos_violencia %>% mutate(Provincia = clasificar_circuito(NombreCircuito))


# agregar flag si es area metropolitana (san jose, alajuela, heredia, cartago) o no (puntarenas, guanacaste, limon)
datos_violencia <- datos_violencia %>% mutate(AreaMetropolitana = ifelse(Provincia %in% c("San Jose", "Alajuela", "Heredia", "Cartago"), 1, 0))


