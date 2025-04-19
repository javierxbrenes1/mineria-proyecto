source("utilitarios.r")

options(width = 2000) 
# Cargar datos de violencia doméstica
datos_violencia <- cargar_datos_violencia()


variables_numericas <- datos_violencia %>%
  select(where(is.numeric), -Anno, -CirculanteInicialLeg, -Legajos, -AbandonadosOInactivos, -CirculanteFinalLegajos)
variables_numericas


df <- scale(variables_numericas)
head(df)

distancias <- dist(df, method = "euclidean")
fviz_dist(distancias, gradient = list(low = "blue", mid = "white", high = "red"))


fviz_nbclust(df, kmeans, method = "silhouette")

k2 <- kmeans(df, centers = 2, nstart = 25)
fviz_cluster(k2, data = df)
