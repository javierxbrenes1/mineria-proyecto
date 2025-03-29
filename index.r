##HIPOTESIS

# Analizar concentraciones geofraficas por edad y actividad. (Mapa de calor, clustering)

#Que tanta diferencia hay entre el GAM y fuera del GAM comparando las diferentes provincias
#Es decir, ¿hay alguna diferencia entre las zonas urbanas o rurales o se mantienen de forma similar?


##VIOLENCIA DOMESTICA

####### HIPOTESIS
#Exite alguna tendencia o mes donde los reportes de violencia se disparen

#Si hay mas testimonios clave ayuda a cerrar mas casos?

#Hay mas casos de violencia en zonas rurales que en zonas urbanas?

#Cuales son los circuitos que mas tardan en cerrar casos y cuales son los mas eficientes?

##CORRELACION
#Existe alguna CORRELACION ENTRE LOS TESTIMONIOS CLAVE contra los casos cerrados

install.packages("readxl")
library(readxl)
library(dplyr)

archivo <- file.choose() #Seleccionar archivo xls

dataViolenciaDomestica <- read_excel("ARCHIVO VIOLENCIA DOMESTICA.xls")

head(dataViolenciaDomestica)


#Exite alguna tendencia o mes donde los reportes de violencia se disparen
#Validando expedientes ingresados durante ese mes
dataViolenciaDomestica %>% 
  group_by(Anno, Mes, NombreCircuito) %>% 
  summarise(Entrados) %>% 
  arrange(desc(Entrados))

#Validando expedientes activos durante el mes
dataViolenciaDomestica %>% 
  group_by(Anno, Mes, NombreCircuito) %>% 
  summarise(CirculanteInicial) %>% 
  arrange(desc(CirculanteInicial))










