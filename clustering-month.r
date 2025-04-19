source("utilitarios.r")

options(width = 2000) 
# Cargar datos de violencia doméstica
datos_violencia <- cargar_datos_violencia()

# Preparar datos para clustering por mes
datos_por_mes <- datos_violencia %>%
  group_by(Mes) %>%
  summarise(
    Entrados = mean(Entrados),
    Terminados = mean(Terminasdos),
    CirculanteInicial = mean(CirculanteInicial),
    CirculanteFinal = mean(CirculanteFinal)
  )
datos_por_mes
# Escalar los datos
df_mes <- scale(datos_por_mes[,-1])  # Excluimos la columna Mes al escalar
rownames(df_mes) <- datos_por_mes$Mes
df_mes
# Calcular distancias
distancias <- dist(df_mes, method = "euclidean")
fviz_dist(distancias, gradient = list(low = "blue", mid = "white", high = "red"))

# Encontrar número óptimo de clusters
fviz_nbclust(df_mes, kmeans, method = "silhouette")

# Realizar clustering con k=3
k_mes <- kmeans(df_mes, centers = 3, nstart = 25)

# Visualizar clusters
fviz_cluster(k_mes, data = df_mes,
             main = "Clusters de Meses por Patrones de Violencia Doméstica",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

# Agregar etiquetas de meses a los datos originales
datos_por_mes$Cluster <- k_mes$cluster
print("Agrupación de meses por cluster:")
print(datos_por_mes %>% arrange(Cluster))
