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
# Cargar datos de violencia doméstica
datos_violencia <- cargar_datos_violencia()

# Verificar la estructura de los datos
str(datos_violencia)
head(datos_violencia)
