# circuitos_provincias <- data.frame(
#   circuito = c(
#     "I Circuito Judicial de San José",
#     "II Circuito Judicial de San José",
#     "III Circuito Judicial de San José",
#     "I Circuito Judicial de la Zona Sur",
#     "II Circuito Judicial de la Zona Sur",
#     "I Circuito Judicial de Alajuela",
#     "II Circuito Judicial de Alajuela",
#     "III Circuito Judicial de Alajuela",
#     "Circuito Judicial de Cartago",
#     "Circuito Judicial de Heredia",
#     "I Circuito Judicial de Guanacaste",
#     "II Circuito Judicial de Guanacaste",
#     "Circuito Judicial de Puntarenas",
#     "I Circuito Judicial de la Zona Atlántica",
#     "II Circuito Judicial de la Zona Atlántica"
#   ),
#   provincia = c(
#     "San Jose",
#     "San Jose",
#     "San Jose",
#     "Puntarenas",
#     "Puntarenas",
#     "Alajuela",
#     "Alajuela",
#     "Alajuela",
#     "Cartago",
#     "Heredia",
#     "Guanacaste",
#     "Guanacaste",
#     "Puntarenas",
#     "Limon",       # Zona Atlántica is Limón
#     "Limon"        # Zona Atlántica is Limón
#   )
# )

circuitos_provincias <- data.frame(
    circuito = limpiar_texto(c(
        'San Jose',
        'Heredia',
        'Alajuela',
        'Cartago',
        'Guanacaste',
        'Puntarenas',
        'Limon',
        'ATLÁNTICA',
        'Sur',
        'ATENAS',
        'Turrialba',
        'Golfito'
        
    )),
    provincia = c(
        'San Jose',
        'Heredia',
        'Alajuela',
        'Cartago',
        'Guanacaste',
        'Puntarenas',
        'Limon',
        'Limon',
        'Puntarenas',
        'Alajuela',
        'Cartago',
        'Puntarenas'
    )
)

circuitos_provincias

# Function to load domestic violence data
cargar_datos_violencia <- function(ruta = "data_sources/ARCHIVO_VIOLENCIA_DOMESTICA.tsv") {
  # Validate if file exists
  if (!file.exists(ruta)) {
    stop("Error: El archivo no existe en la ruta especificada: ", ruta)
  }
  

    # Read CSV file
    datos <- read.csv(ruta, 
                     stringsAsFactors = FALSE, 
                     encoding = "UTF-8",
                     check.names = FALSE,
                     sep = '\t')

    return(datos)
}

limpiar_texto <- function(texto) {
  texto <- tolower(texto)
  texto <- stri_trans_general(texto, "Latin-ASCII")
  texto <- str_trim(texto)
  return(texto)
}

clasificar_circuito <- function(circuitos) {
    resultados <- rep(NA_character_, length(circuitos))
    
    for(i in seq_len(nrow(circuitos_provincias))) {
        matches <- str_detect(limpiar_texto(circuitos), limpiar_texto(circuitos_provincias$circuito[i]))
        resultados[matches] <- circuitos_provincias$provincia[i]
    }
    return(resultados)
}
