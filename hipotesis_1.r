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
library(ggplot2)
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
datos_violencia <- datos_violencia %>% mutate(AreaMetropolitana = ifelse(Provincia %in% c("San Jose", "Alajuela", "Heredia", "Cartago"), 'SI', 'NO'))

## Casos nuevos

ggplot(datos_violencia, aes(x = AreaMetropolitana, fill = Entrados)) +
  geom_bar() +
    labs(title = "Distribución de casos de violencia doméstica por área metropolitana",
       x = "Área Metropolitana",
       y = "Número de casos Entrados", )


ggplot(datos_violencia, aes(x = Provincia, fill = as.factor(AreaMetropolitana))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribución de casos de violencia doméstica por provincia y área metropolitana",
       x = "Provincia",
       y = "Número de casos")




ggplot(datos_violencia, aes(x = Entrados, fill = AreaMetropolitana)) +
  geom_histogram(position = "dodge", binwidth = 50) +
 facet_wrap(~Anno) +
  labs(
    title = "Distribución de Casos Entrados por Área Metropolitana y Año",
    x = "Número de Casos Entrados",
    y = "Frecuencia",
    fill = "Área Metropolitana"
  ) +
 theme_minimal() +
  scale_fill_brewer(palette = "Set3") + 
    scale_x_continuous(
    breaks = seq(0, max(datos_violencia$Entrados), by = 100),  # Shows breaks every 100
    labels = scales::comma  # Formats numbers with commas for readability
  )


## casos terminados

ggplot(datos_violencia, aes(x = Terminasdos, fill = AreaMetropolitana)) +
  geom_histogram(position = "dodge", binwidth = 50) +
  facet_wrap(~Anno) +
  labs(
    title = "Distribución de Casos Terminados por Área Metropolitana y Año",
    x = "Número de Casos Terminados",
    y = "Frecuencia",
    fill = "Área Metropolitana"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") + 
    scale_x_continuous(
    breaks = seq(0, max(datos_violencia$Terminasdos), by = 100),  # Shows breaks every 100
    labels = scales::comma  # Formats numbers with commas for readability
  )

ggplot(datos_violencia, aes(x = AreaMetropolitana, fill = Terminasdos)) +
  geom_bar() +
    labs(title = "Distribución de casos terminados de violencia doméstica por área metropolitana",
       x = "Área Metropolitana",
       y = "Número de casos Terminados", )
