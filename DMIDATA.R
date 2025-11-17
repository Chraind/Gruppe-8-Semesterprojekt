pacman::p_load(
  "tidyverse", "arrow", "babynames", "curl", "duckdb", "gapminder", 
  "ggrepel", "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", 
  "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", 
  "repurrrsive", "tidymodels", "writexl", "arrow", "scales", "patchwork",
  "ggforce", "concaveman","httr","rjson","rjstat","rlist","jsonlite")

# Basis-URL og parametre
base_url <- "https://dmigw.govcloud.dk/v2/"
info_url <- "metObs/collections/observation/items?"
stationId <- "stationId=06065"  # aars syd station
apikey <- "api-key=08390a45-0336-470a-bb2e-8b36e59989c2"

# Læs kampdatoer
datoer <- read.csv("kamp_dato.csv")#placeholder dato fra supstats data

# Samlet dataframe
all_data <- data.frame()

# Loop over datoer
for (date in datoer$kamp_dato) {
  # Byg datetime-parameter
  datetime <- paste0("&datetime=", as.character(date), "T12:00:00Z/", as.character(date), "T12:00:00Z&")
  
  # Byg URL
  url <- paste0(base_url, info_url, stationId, datetime, apikey)
  message("Henter data fra: ", url)
  
  # Hent data
  res <- GET(url)
  json_txt <- rawToChar(res$content)
  
  # Tjek om responsen er tom
  if (nchar(json_txt) > 0) {
    data <- fromJSON(json_txt, simplifyVector = FALSE)
    
    if (length(data$features) > 0) {
      df <- map_df(data$features, ~ as.data.frame(.x$properties))
      df$kamp_dato <- date  # Tilføj kampdato til data
      all_data <- bind_rows(all_data, df)
    } else {
      message("Ingen data for ", date)
    }
  } else {
    message("Tom respons fra API for ", date)
  }
}


wide_data <- all_data %>%
  dplyr::select(kamp_dato, parameterId, value) %>%
  group_by(kamp_dato, parameterId) %>%
  summarise(value = first(value), .groups = "drop") %>%
  pivot_wider(names_from = parameterId, values_from = value)

names(wide_data)

