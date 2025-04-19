# 3. Eficiencia en la Resolución de Casos:
# Pregunta de Investigación: 
# ¿Cuál es la eficiencia de los diferentes circuitos judiciales en la resolución de casos de violencia doméstica en el 2024? 
# ¿Existen circuitos con un desempeño significativamente superior en el cierre de casos?

# Objetivos:
# Evaluar la eficiencia de los despachos judiciales en el cierre de casos de violencia doméstica.
# Identificar los circuitos judiciales con mayor y menor eficiencia en la resolución de casos.

# Requerimientos de Datos:
# Utilización del conjunto de datos existente para analizar la relación entre los casos ingresados y los casos terminados por circuito judicial.

# Load required libraries first
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)

# Set options
options(width = 2000) 

# Source utility functions
source("utilitarios.r")

# Cargar y filtrar datos para 2024
dv <- cargar_datos_violencia()
dv <- dv %>% filter(Anno == 2024) %>% 
  select(NombreMes, NombreCircuito, CirculanteInicial, Entrados, Terminasdos)

# Calcular métricas de eficiencia
dv_eficiencia <- dv %>%
  group_by(NombreCircuito) %>%
  summarise(
    Total_Circulante_Inicial = sum(CirculanteInicial),
    Total_Entrados = sum(Entrados),
    Total_Terminados = sum(Terminasdos),
    Carga_Total = Total_Circulante_Inicial + Total_Entrados,
    # Diferentes métricas de eficiencia
    # cuantos casos fueron resueltos comparados al total de trabajo (circulantes + entrados), valor alto indica mejor eficiencia en manejo de caso
    Eficiencia_Carga_Total = (Total_Terminados / Carga_Total) * 100, 
    # que tanto manejan el backlog, mejor porcentaje indica mejor forma de manejar casos pendientes
    Tasa_Resolucion_Pendientes = (Total_Terminados / Total_Circulante_Inicial) * 100
  ) %>%
  mutate(
    across(ends_with("_Total") | starts_with("Tasa"), round, 2),
    Circulante_Final = Total_Circulante_Inicial + Total_Entrados - Total_Terminados,
    Reduccion_Pendientes = ((Total_Circulante_Inicial - Circulante_Final) / Total_Circulante_Inicial) * 100
  ) %>%
  arrange(desc(Eficiencia_Carga_Total))

# Mostrar resultados
print("Análisis de Eficiencia por Circuito Judicial (2024):")
print(dv_eficiencia, n = nrow(dv_eficiencia))

# Visualizar las diferentes métricas de eficiencia
dv_eficiencia_long <- dv_eficiencia %>%
  select(NombreCircuito, Eficiencia_Carga_Total, Tasa_Resolucion_Pendientes) %>%
  tidyr::pivot_longer(
    cols = c(Eficiencia_Carga_Total, Tasa_Resolucion_Pendientes),
    names_to = "Tipo_Eficiencia",
    values_to = "Porcentaje"
  )

dv_eficiencia %>% filter(str_detect(limpiar_texto(NombreCircuito), "santa cruz")) %>% select(NombreCircuito)

ggplot(dv_eficiencia_long, aes(x = reorder(NombreCircuito, Porcentaje), y = Porcentaje, fill = Tipo_Eficiencia)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Métricas de Eficiencia por Circuito Judicial (2024)",
    x = "Circuito Judicial",
    y = "Porcentaje",
    fill = "Tipo de Eficiencia"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(size = 10)
  ) +
  scale_fill_brewer(palette = "Set2",
    labels = c(
      "Eficiencia_Carga_Total" = "Eficiencia sobre Carga Total",
      "Tasa_Resolucion_Pendientes" = "Tasa de Resolución de Pendientes"
    ))

# Identificar circuitos más y menos eficientes
top_circuitos <- dv_eficiencia %>%
  slice_max(Eficiencia_Carga_Total, n = 3)

bottom_circuitos <- dv_eficiencia %>%
  slice_min(Eficiencia_Carga_Total, n = 3)

print("\nCircuitos más eficientes:")
print(top_circuitos)

print("\nCircuitos menos eficientes:")
print(bottom_circuitos)
