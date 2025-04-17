source("utilitarios.r")
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(tidyr)  # For pivot_longer
options(width = 2000) 
# Cargar datos de violencia doméstica
datos_violencia <- cargar_datos_violencia()

# Function to calculate mode
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Select only numeric columns and calculate statistics
solo_enteros <- datos_violencia %>% 
  select(where(is.numeric)) %>%
  summarise(across(everything(), 
    list(
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      mode = ~calculate_mode(.)
    )
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "statistic"),
    names_pattern = "(.*)_(.*)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

# Display the results
print(solo_enteros, n = Inf)


