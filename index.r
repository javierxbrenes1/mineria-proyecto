
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
