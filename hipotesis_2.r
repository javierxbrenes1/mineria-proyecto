# 2. Temporalidad de la Violencia Doméstica:
# Pregunta de Investigación: 
# ¿Se identifican periodos específicos del año con un aumento significativo en la presentación de casos de violencia doméstica?

# Objetivos:
# Determinar los periodos del año (meses) con mayor incidencia de casos nuevos de violencia doméstica.
# Analizar la evolución de los casos nuevos a lo largo de los años.

# Requerimientos de Datos:
# Definición de un subconjunto de datos a partir del existente, ordenado cronológicamente por mes y año de ingreso de los casos.



source("utilitarios.r")
options(width = 2000) 
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)
source("utilitarios.r")

# Cargar el archivo TSV (ajusta la ruta si es necesario)
data <- cargar_datos_violencia()

# Crear columna Fecha para orden temporal
data <- data %>%
  mutate(
    Fecha = as.Date(paste(Anno, Mes, "01", sep = "-")),
    NombreMes = factor(NombreMes, 
                       levels = meses,
                       ordered = TRUE)
  )

data %>% distinct(NombreMes)

# Corregir valores 'N/A' por 'Febrero'
data$NombreMes[is.na(data$NombreMes) | data$NombreMes == "N/A"] <- "Febrero"

##COMENTARIO:
## Se cargan liberias, data y se agrega una columna con los meses correspondientes a cada observación.
## Además se corrigen los valores de Febrero que se muestran como N/A 


# Gráfico 1: Casos por mes y año (barras)
ggplot(data, aes(x = NombreMes, y = Entrados, fill = factor(Anno))) +
  geom_col(position = "dodge") +
  labs(
    title = "Casos Nuevos de Violencia Doméstica por Mes y Año",
    x = "Mes",
    y = "Casos Nuevos",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##COMENTARIO:
## Este gráfico muestra un conteo mensual desde el año 2015 al 2025 con el total de nuevos casos de violencia
##Es importante destacar que los datos proporcionados del 2025 llegan solamente del mes de Enero


#Gráfico 2: Promedio mensual de casos por año (líneas)

library(dplyr)

data %>%
  group_by(Anno, NombreMes) %>%
  summarise(Entrados = mean(Entrados), .groups = 'drop') %>%
  ggplot(aes(x = NombreMes, y = Entrados, group = Anno, color = factor(Anno))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Promedio Mensual de Casos Nuevos por Año",
    x = "Mes",
    y = "Casos Nuevos",
    color = "Año"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##COMENTARIO
## En este gráfico podemos ver el promedio mensual de nuevos casos por cada año.
##Encontramos que el 2015 y 2016 sobresalen del promedio de los otros años


# Gráfico 3: Evolución anual total de casos
data %>%
  group_by(Anno) %>%
  summarise(TotalAnual = sum(Entrados)) %>%
  ggplot(aes(x = Anno, y = TotalAnual)) +
  geom_line(group = 1, color = "firebrick", linewidth = 1.2) +
  geom_point(size = 3, color = "firebrick") +
  labs(
    title = "Evolución Anual de Casos Nuevos de Violencia Doméstica",
    x = "Año",
    y = "Total de Casos"
  ) +
  theme_minimal()

##COMENTARIO:
## En este gráfico se contabilizan todos los casos nuevos de cada año 
## Se encuentra que desde el 2015 hasta 2019 hay una tendencia a la alza de casos y luego cae levemente


# Gráfico 4: Total acumulado por mes (todos los años)

data %>%
  group_by(NombreMes) %>%
  summarise(TotalCasos = sum(Entrados)) %>%
  ggplot(aes(x = NombreMes, y = TotalCasos)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Total de Casos Nuevos por Mes (Todos los Años Combinados)",
    x = "Mes",
    y = "Total de Casos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##COMENTARIO:
## Este gráfico muestra el total de casos nuevos separados por mes
## Se confirma que en los últimos 10 años el mes con mas casos de denuncias por violencia son los de febrero en primer lugar, enero en segundo puesto y los demás meses se mantienen muy similares entre ellos

