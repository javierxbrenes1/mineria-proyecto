source("utilitarios.r")
library(forecast)
library(randomForest)
library(caret)
library(ggplot2)
library(tidyr)

# Cargar y preparar datos
datos_violencia <- cargar_datos_violencia()

# 1. Análisis de Series Temporales
# Preparar datos mensuales
datos_ts <- datos_violencia %>%
  group_by(Anno, Mes) %>%
  summarise(
    Entrados = sum(CirculanteInicial) + sum(Entrados),
    Terminados = sum(Terminasdos)
  ) %>%
  arrange(Anno, Mes)

# Crear serie temporal y descomposición
ts_entrados <- ts(datos_ts$Entrados, frequency = 12)
descomposicion <- decompose(ts_entrados)

# Convertir la descomposición a data frame para ggplot
tiempo <- seq_along(ts_entrados)
df_descomposicion <- data.frame(
  tiempo = tiempo,
  observado = as.numeric(descomposicion$x),
  tendencia = as.numeric(descomposicion$trend),
  estacional = as.numeric(descomposicion$seasonal),
  aleatorio = as.numeric(descomposicion$random)
) %>%
  tidyr::pivot_longer(cols = -tiempo, names_to = "componente", values_to = "valor")

# Visualizar descomposición con ggplot
ggplot(df_descomposicion, aes(x = tiempo, y = valor)) +
  geom_line() +
  facet_wrap(~componente, scales = "free_y", ncol = 1) +
  labs(
    title = "Descomposición de Serie Temporal - Casos de Violencia Doméstica",
    x = "Tiempo",
    y = "Valor"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    strip.text = element_text(size = 10)
  )

# Pronóstico con ARIMA
modelo_arima <- auto.arima(ts_entrados)
pronostico <- forecast(modelo_arima, h = 12)

# Convertir pronóstico a data frame para ggplot
df_pronostico <- data.frame(
  tiempo = c(tiempo, max(tiempo) + 1:12),
  valor = c(ts_entrados, pronostico$mean),
  tipo = c(rep("Histórico", length(ts_entrados)), rep("Pronóstico", 12)),
  lower = c(rep(NA, length(ts_entrados)), pronostico$lower[,2]),
  upper = c(rep(NA, length(ts_entrados)), pronostico$upper[,2])
)

# Visualizar pronóstico con ggplot
ggplot(df_pronostico, aes(x = tiempo, y = valor)) +
  geom_line(aes(color = tipo)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    title = "Pronóstico de Casos de Violencia Doméstica",
    x = "Tiempo",
    y = "Número de Casos",
    color = "Tipo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Histórico" = "black", "Pronóstico" = "blue"))

# 2. Modelo de Regresión Random Forest
datos_rf <- datos_violencia %>%
  select(Mes, CirculanteInicial, Entrados, Terminasdos) %>%
  mutate(
    Mes = as.factor(Mes)
  )

# Dividir datos
set.seed(123)
indice_entrenamiento <- createDataPartition(datos_rf$Entrados, p = 0.7, list = FALSE)
datos_entrenamiento <- datos_rf[indice_entrenamiento,]
datos_prueba <- datos_rf[-indice_entrenamiento,]

# Entrenar modelo Random Forest
modelo_rf <- randomForest(
  Entrados ~ .,
  data = datos_entrenamiento,
  ntree = 500,
  importance = TRUE
)

# Convertir importancia de variables para ggplot
importancia <- importance(modelo_rf)
df_importancia <- data.frame(
  variable = rownames(importancia),
  importancia = importancia[,1]
) %>%
  arrange(desc(importancia))

# Visualizar importancia de variables con ggplot
ggplot(df_importancia, aes(x = reorder(variable, importancia), y = importancia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de Variables en Predicción de Casos Entrados",
    x = "Variables",
    y = "Importancia (%IncMSE)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

# Predicciones y evaluación
predicciones <- predict(modelo_rf, datos_prueba)
rmse <- sqrt(mean((predicciones - datos_prueba$Entrados)^2))
r2 <- 1 - sum((datos_prueba$Entrados - predicciones)^2) / 
        sum((datos_prueba$Entrados - mean(datos_prueba$Entrados))^2)

print(paste("RMSE:", round(rmse, 2)))
print(paste("R-squared:", round(r2, 2)))

# Visualizar predicciones vs valores reales
plot_data <- data.frame(
  Real = datos_prueba$Entrados,
  Predicho = predicciones
)

ggplot(plot_data, aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Valores Reales vs Predicciones",
    x = "Casos Reales",
    y = "Casos Predichos"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

# 3. Análisis de Tendencias Mensuales
tendencias_mensuales <- datos_violencia %>%
  group_by(Mes) %>%
  summarise(
    Promedio_Entrados = mean(Entrados),
    SD_Entrados = sd(Entrados)
  )

ggplot(tendencias_mensuales, aes(x = Mes, y = Promedio_Entrados)) +
  geom_line(group = 1, color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbar(aes(ymin = Promedio_Entrados - SD_Entrados,
                    ymax = Promedio_Entrados + SD_Entrados),
                width = 0.2, color = "steelblue", alpha = 0.5) +
  labs(
    title = "Tendencia Mensual de Casos de Violencia Doméstica",
    x = "Mes",
    y = "Promedio de Casos Entrados"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    panel.grid.minor = element_blank()
  ) 