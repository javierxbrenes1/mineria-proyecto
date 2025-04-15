
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
