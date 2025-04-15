# 1. Distribución Geográfica de la Violencia Doméstica:

# Pregunta de Investigación:
# ¿Existe una variación significativa en la incidencia de casos de violencia doméstica entre áreas urbanas y rurales?

# Objetivos:
# Analizar la distribución geográfica de los casos nuevos y terminados de violencia doméstica por circuito judicial.
# Realizar una comparación de la incidencia de casos entre zonas urbanas y rurales.

# Requerimientos de Datos:
# Depuración y homogeneización del conjunto de datos existente.
# Enriquecimiento del conjunto de datos para permitir la clasificación de los circuitos judiciales en zonas urbanas y rurales.

# Importar funciones utilitarias
source("utilitarios.r")
options(width = 2000) 
# Cargar datos de violencia doméstica
datos_violencia <- cargar_datos_violencia()

# Verificar la estructura de los datos
str(datos_violencia)
head(datos_violencia)


