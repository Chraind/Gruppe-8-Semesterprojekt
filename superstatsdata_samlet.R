pacman::p_load(tidyverse, janitor, rvest, lubridate, stringi, httr, jsonlite, purrr)

### Find de år hvor VFF er i superligaen
superliga_url <- "https://superstats.dk/hold/alltime?id=11"
read_superliga <- read_html(superliga_url, encoding = "UTF-8")

# Læs den korrekte table, fundet ved at kigge på HTML siden
# Tilføj kolonne og fjern tekst før / i skemaet
superliga_table_read <- read_superliga %>% 
  html_element("table#sortTable") %>% 
  html_table(fill = TRUE) %>% 
  mutate(
    sæson_år = str_extract(Sæson, "(?<=/)[0-9]{4}") %>% as.integer()
  )

sæson_år <- superliga_table_read %>% 
  filter(!is.na(sæson_år) & sæson_år >= 2003) %>% 
  pull(sæson_år)
#       sæson_år returnerer
#       [1] 2026 2025 2024 2023 2022 2017 2016 2014 2008 2007 2006 2005 2004 2003

min_url <- "https://superstats.dk/program?season="
sæson_urls <- c()
sæson_urls <- paste0(min_url, sæson_år)

kombineret_runde_table <- tibble()
# tryCatch er error handling - sker der en fejl kører den koden i error=function(e) i stedet
tryCatch({
  # tilføjer en pause mellem requests (1) = 1 sek for at undgå overbelastning af serveren
  Sys.sleep(1)
  
for (i in seq_along(sæson_urls)) {
  url <- sæson_urls[i]
  page <- read_html(url, encoding = "UTF-8")
  individuel_sæson <- sæson_år[i]
  
  # Extract all tables as HTML nodes
  table_nodes <- page %>% html_nodes("table")
  
  for (tbl_node in table_nodes) {
    df <- html_table(tbl_node, fill = TRUE)
    
    # Ignorer alle skemaer hvor "Runde" ikke er inkluderet
    if (!any(grepl("Runde", names(df), ignore.case = TRUE))) next
    
    # TV kanal info
    tv_titles <- tbl_node %>% html_nodes("td img[title]") %>% html_attr("title")
    if (length(tv_titles) == nrow(df)) df$tv <- tv_titles else df$tv <- NA_character_
    
    df$sæson <- individuel_sæson
    
    # Clean names & rename, indsæt kolonnenavne
    df <- df %>% clean_names()
    col_names <- c("ugedag","dato","matchup","stilling","tilskuere","dommer","delete","tv_kanal","sæson")
    names(df)[1:min(length(col_names), ncol(df))] <- col_names[1:min(length(col_names), ncol(df))]
    
    # Fix Tilskuere, fjern punktum
    if ("tilskuere" %in% names(df)) {
      df <- df %>%
        mutate(
          tilskuere = gsub("\\.","",tilskuere),
          tilskuere = gsub(",",".",tilskuere),
          tilskuere = as.numeric(tilskuere)
        )
    }
    # Tilføj til tibble
    kombineret_runde_table <- bind_rows(kombineret_runde_table, df)
  }
}
}, error = function(e) {
  # Denne del kører kun hvis der sker en fejl
  cat(" Fejl ved hentning af data for", year, ":", e$message, "\n")  
})

# Gem RDS så man ikke ddos'er superstats, da man kun behøver køre ovenstående kode når man vil have opdateret tabel
SaveRDS(kombineret_runde_table, "kombineret_runde_table.rds")

# Load RDS
kombineret_runde_table <- readRDS("kombineret_runde_table.rds")


# TODO: upload scraped data til SQL databasen

# ---- date.nager.at data hentning ----
alle_helligdage <- tibble()
for (year in sæson_år) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", year, "/DK")
  response <- GET(url)
  helligdage <- fromJSON(content(response, "text", encoding = "UTF-8"))
  alle_helligdage <- bind_rows(alle_helligdage, helligdage)
  Sys.sleep(1)
}

# ---- DATARENSNING ----
vff_kampdata_clean <- kombineret_runde_table %>%
  # Split dato_tid i separate kolonner
  separate(dato, into = c("dato", "tid"), sep = " ", remove = FALSE) %>%
  
  # Identificer VFF hjemmekampe
  # Hjemmeholdet står først i "matchup" kolonnen (f.eks. "VFF-FCK" eller "Viborg FF - FCK")
  mutate(
    # Tjek om VFF/Viborg er hjemmehold (står før bindestreg)
    vff_hjemme = grepl("^(VFF|Viborg)", matchup, ignore.case = TRUE)
  ) %>%
  
  # Filtrer kun VFF hjemmekampe
  filter(vff_hjemme == TRUE) %>%
  
  # Konverter tilskuere til numerisk (fjern punktum-tusindtalsseparator hvis nødvendig)
  mutate(
    tilskuere = as.numeric(gsub("[^0-9]", "", tilskuere))
  ) %>% 
  
  # Vis sæson korrekt
  mutate(sæson = paste0(sæson - 1, "/", sæson)) %>% 
  
  # Tilføj år til dato-strengen
  mutate(
    month = as.numeric(str_sub(dato, 4, 5)),  # Gemmer position 4 og 5 i dato kolonnen, ekstraherer måned fra "18/07"
    end_year = as.numeric(str_sub(sæson, 6, 9)),  # Gemmer position 6 til 9 i sæson kolonnen, som er 2026 i "2025/2026"
    år = ifelse(month >= 7 & month <= 12, end_year - 1, end_year)
  ) %>%
  
  # Tilføj match_dato
  mutate (
    match_dato = dmy(paste0(dato, "/", år))
  ) %>% 
  
  # Indsættelse af helligdag data fra date.nager.at og ser om datoen matcher match_dato
  mutate(
    helligdag = match_dato %in% as.Date(alle_helligdage$date)
  ) %>% 
  
  # Fjern unødvendige kolonner
dplyr::select(-delete, -month, -end_year) %>%
  
  # Omorganiser kolonner
dplyr::select(sæson, år, ugedag, dato, tid, matchup, stilling, tilskuere, dommer, tv_kanal, match_dato, helligdag)

# Se resultatet
view(vff_kampdata_clean)
str(vff_kampdata_clean)





