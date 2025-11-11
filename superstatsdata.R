pacman::p_load(tidyverse, ggthemes, ggridges, janitor, vroom, arrow, readxl, writexl, purrr, 
               stringi, hms, tidymodels, MASS, tseries, ggfortify, polite, tufte, rvest, lubridate)
###
### Find de år hvor VFF er i superligaen
###

superliga_url <- "https://superstats.dk/hold/alltime?id=11"
read_superliga <- read_html(superliga_url, encoding = "UTF-8")

# superliga_table <- superliga_url %>% 
  #read_html(encoding = "UTF-8") %>% 
  #html_nodes("table")

# Læs den korrekte table, fundet ved at kigge på HTML siden
superliga_table_read <- read_superliga %>% 
  html_element("table#sortTable") %>% 
  html_table(fill = TRUE)

superliga_sæsoner <- superliga_table_read %>%
  mutate(
    sæson_år = str_extract(Sæson, "(?<=/)[0-9]{4}") %>% as.integer()
  )

sæson_år <- superliga_sæsoner %>% 
  filter(!is.na(sæson_år) & sæson_år >= 2003) %>% 
  pull(sæson_år)
#       sæson_år returnerer
#       [1] 2026 2025 2024 2023 2022 2017 2016 2014 2008 2007 2006 2005 2004 2003

# Finder alle kampe hvor VFF er i superligaen
min_url <- "https://superstats.dk/program?season="
sæson_urls <- c()

sæson_urls <- paste0(min_url, sæson_år)
sæson_urls

kombineret_runde_table <- tibble()

###
### MOTHER OF ALL FOR LOOPS
###

for (i in seq_along(sæson_urls)) {
  url <- sæson_urls[i]
  page <- read_html(url, encoding = "UTF-8")
  individuel_sæson <- sæson_år[i]
  
  # Extract all tables as HTML nodes
  table_nodes <- page %>% html_nodes("table")
  
  for (tbl_node in table_nodes) {
    df <- html_table(tbl_node, fill = TRUE)
    
    # Only keep tables with "Runde" in column names
    if (!any(grepl("Runde", names(df), ignore.case = TRUE))) next
    
    # TV info
    tv_titles <- tbl_node %>% html_nodes("td img[title]") %>% html_attr("title")
    if (length(tv_titles) == nrow(df)) df$tv <- tv_titles else df$tv <- NA_character_
    
    df$sæson <- individuel_sæson
    
    # Clean names & rename
    df <- df %>% clean_names()
    col_names <- c("ugedag","dato","matchup","stilling","tilskuere","judge","delete","tv_kanal","sæson")
    names(df)[1:min(length(col_names), ncol(df))] <- col_names[1:min(length(col_names), ncol(df))]
    
    # Fix Tilskuere
    if ("tilskuere" %in% names(df)) {
      df <- df %>%
        mutate(
          tilskuere = gsub("\\.","",tilskuere),
          tilskuere = gsub(",",".",tilskuere),
          tilskuere = as.numeric(tilskuere)
        )
    }
    
    # Parse date
    if ("dato" %in% names(df)) {
      df <- df %>%
        mutate(
          dato = paste0(dato,"/",individuel_sæson),
          dato = gsub("([0-9]{2}/[0-9]{2}) ([0-9]{2}:[0-9]{2})/([0-9]{4})","\\1/\\3 \\2",dato),
          dato = suppressWarnings(lubridate::dmy_hm(dato))
        )
    }
    
    # Remove unwanted columns (7 and 9) safely
    cols_to_remove <- intersect(c(7,9), seq_along(df))
    if (length(cols_to_remove) > 0) df <- df[,-cols_to_remove]
    
    # Append to main combined table
    kombineret_runde_table <- bind_rows(kombineret_runde_table, df)
  }
}

# Inspect
#view(kombineret_runde_table)

# Gem RDS så man ikke ddos'er superstats
saveRDS(kombineret_runde_table, "kombineret_runde_table.rds")

# Load RDS
kombineret_runde_table <- readRDS("kombineret_runde_table.rds")

view(kombineret_runde_table)

# TODO: upload scraped data til SQL databasen

#save





