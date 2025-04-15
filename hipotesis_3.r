# 3. Eficiencia en la Resolución de Casos:
# Pregunta de Investigación: 
# ¿Cuál es la eficiencia de los diferentes circuitos judiciales en la resolución de casos de violencia doméstica? 
# ¿Existen circuitos con un desempeño significativamente superior en el cierre de casos?

# Objetivos:
# Evaluar la eficiencia de los despachos judiciales en el cierre de casos de violencia doméstica.
# Identificar los circuitos judiciales con mayor y menor eficiencia en la resolución de casos.

# Requerimientos de Datos:
# Utilización del conjunto de datos existente para analizar la relación entre los casos ingresados y los casos terminados por circuito judicial.



source("utilitarios.r")
options(width = 2000) 
# Cargar datos de violencia doméstica
datos_violencia <- cargar_datos_violencia()

# Verificar la estructura de los datos
str(datos_violencia)
head(datos_violencia)