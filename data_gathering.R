pacman::p_load(tidyverse, vroom, janitor, polite, rjstat, rvest, lubridate,
               stringi, httr, jsonlite, purrr, utils, RSQLite, DBI)

# ---- HENT DATA FRA SUPERSTATS ---- #
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
    
    # Ekstraher alle tables som HTML nodes
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
      col_names <- c("ugedag","dato","kamp","stilling","tilskuere","dommer","delete","tv_kanal","sæson")
      names(df)[1:min(length(col_names), ncol(df))] <- col_names[1:min(length(col_names), ncol(df))]
      
      # Ekstraher tilskuere som tekst direkte fra HTML
      tilskuere_text <- tbl_node %>% 
        html_nodes("td:nth-child(5)") %>%
        html_text(trim = TRUE)
      
      if (length(tilskuere_text) == nrow(df)) {
        df$tilskuere <- as.numeric(gsub("\\.", "", tilskuere_text))
      }
      
      # Find runde tekst og indsæt som ny kolonne
      runde_text <- tbl_node %>% 
        html_node("thead") %>% 
        html_text(trim = TRUE)
      df$runde <- as.character(stringr::str_extract(runde_text, "Runde\\s*\\d+"))
      
      # Tilføj til tibble
      kombineret_runde_table <- bind_rows(kombineret_runde_table, df)
    }
  }
}, error = function(e) {
  # Denne del kører kun hvis der sker en fejl
  cat(" Fejl ved hentning af data for", year, ":", e$message, "\n")  
})

# ---- HENT DATA FRA DATE.NAGER.AT ---- #
alle_helligdage <- tibble()
for (year in sæson_år) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", year, "/DK")
  response <- GET(url)
  helligdage <- fromJSON(content(response, "text", encoding = "UTF-8")) %>% 
    select(date, localName)
  alle_helligdage <- bind_rows(alle_helligdage, helligdage)
  Sys.sleep(1)
}

view(alle_helligdage)

# ---- HENT BEFOLKNINGSDATA FRA DANMARKS STATISTIK ---- #
# Definer URLs
urls <- list(
  for_2005 = "https://api.statbank.dk/v1/data/bef1a/JSONSTAT?OMRÅDE=791%2C761%2C763%2C769%2C775%2C789&Tid=2002%2C2003%2C2004",
  år_2005_2007 = "https://api.statbank.dk/v1/data/bef1a07/JSONSTAT?OMRÅDE=791&Tid=2005%2C2006%2C2007",
  efter_2008 = "https://api.statbank.dk/v1/data/folk1a/JSONSTAT?OMRÅDE=791&KØN=TOT&ALDER=IALT&CIVILSTAND=TOT&Tid=2008K1%2C2008K2%2C2008K3%2C2008K4%2C2013K1%2C2013K2%2C2013K3%2C2013K4%2C2014K1%2C2014K2%2C2014K3%2C2014K4%2C2015K1%2C2015K2%2C2015K3%2C2015K4%2C2016K1%2C2016K2%2C2016K3%2C2016K4%2C2017K1%2C2017K2%2C2017K3%2C2017K4%2C2021K1%2C2021K2%2C2021K3%2C2021K4%2C2022K1%2C2022K2%2C2022K3%2C2022K4%2C2023K1%2C2023K2%2C2023K3%2C2023K4%2C2024K1%2C2024K2%2C2024K3%2C2024K4%2C2025K1%2C2025K2%2C2025K3%2C2025K4"
)

# Hent og bearbejd data for 2003-2004 (skal summeres da Kommunen var opdelt anderledes den gang)
stat_viborg_for_2005 <- fromJSONstat(urls$for_2005) %>%
  as_tibble() %>%
  pull(1) %>%
  group_by(tid) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  mutate(
    år = as.character(tid),
    kvartal = as.character(NA)
  ) %>%
  dplyr::select(år, kvartal, value)

# Hent og bearbejd data for 2005-2007
stat_viborg_2005_2007 <- fromJSONstat(urls$år_2005_2007) %>%
  as_tibble() %>%
  pull(1) %>%
  dplyr::select(tid, value) %>%
  mutate(
    år = as.character(tid),
    kvartal = as.character(NA)
  ) %>%
  dplyr::select(år, kvartal, value)

# Hent og bearbejd data efter 2008 (nu med kvartals optælling)
stat_viborg_efter_2008 <- fromJSONstat(urls$efter_2008) %>%
  as_tibble() %>%
  pull(1) %>%
  dplyr::select(tid, value) %>%
  mutate(
    år = str_extract(tid, "^\\d{4}"),
    kvartal = str_extract(tid, "K\\d")
  ) %>%
  dplyr::select(år, kvartal, value)

# Kombiner datasæt til én samlet tibble
viborg_befolkning_komplet <- bind_rows(
  stat_viborg_for_2005,
  stat_viborg_2005_2007,
  stat_viborg_efter_2008
) %>%
  arrange(år, kvartal) %>%
  rename(Indbyggere_Viborg_Kommune = value)

# ---- DATARENSNING ---- #
vff_kampdata_upload <- kombineret_runde_table %>%
  
  # --- Split dato/tid i separate kolonner ---
  separate(dato, into = c("dato", "tid"), sep = " ", remove = FALSE) %>%
  
  # --- Identificer VFF-kampe ---
  mutate(
    vff_spiller = grepl("(VFF|Viborg)", kamp, ignore.case = TRUE)
  ) %>%
  filter(vff_spiller, !is.na(tilskuere)) %>%
  
  # --- Sæson & år ---
  mutate(
    sæson = paste0(sæson - 1, "/", sæson), # vis sæson korrekt
    month = as.numeric(str_sub(dato, 4, 5)), 
    end_year = as.numeric(str_sub(sæson, 6, 9)),
    # Tildel korrekt kalenderår (sæson starter i juli, slutter i maj)
    år = ifelse(month >= 7 & month <= 12, end_year - 1, end_year)
  ) %>%
  
  # --- Dato ---
  mutate(kamp_dato = dmy(paste0(dato, "/", år))) %>%
  
  # --- Tid & tidsgrupper ---
  # Tilføj tids kolonne, tidligt midt sent
  mutate(
    # Formater tid så de står i timer og minutter (HH:MM)
    tid = na_if(tid, ""), # hvis NA eksisterer brug "" i stedet
    klokkeslæt = hms::parse_hm(tid),
    tidsgruppe = case_when(
      klokkeslæt >= hms::parse_hm("12:00") & klokkeslæt < hms::parse_hm("15:30") ~ "tidligt",
      klokkeslæt >= hms::parse_hm("15:30") & klokkeslæt < hms::parse_hm("18:30") ~ "midt",
      klokkeslæt >= hms::parse_hm("18:30") & klokkeslæt <= hms::parse_hm("23:59") ~ "sent"
    )
  ) %>%
  
  # --- Score & resultat ---
  # Tilføj kolonne hvor VFF har vundet sidste kamp
  separate(stilling, into = c("score_home", "score_away"), sep = "-", convert = TRUE, remove = FALSE) %>% 
  
  # Tilføj kolonne med VFF score og modstander score, og navne
  mutate(
    vff_score = ifelse(grepl("^(VFF|Viborg)", kamp), score_home, score_away),
    modstander_score = ifelse(grepl("^(VFF|Viborg)", kamp), score_away, score_home),
    vff_vundet = case_when(
      vff_score > modstander_score ~ "vundet",
      vff_score < modstander_score ~ "tabt",
      TRUE ~ "uafgjort"
    )
  ) %>%
  
  # Identificer om VFF spillede hjemme (kampnavn starter med VFF/Viborg)
  mutate(
    vff_hjemme = grepl("^(VFF|Viborg)", kamp, ignore.case = TRUE)
  ) %>%
  
  #Behold hjemmekampe
  filter(vff_hjemme == TRUE) %>%
  
  # --- Sortér kronologisk ---
  arrange(kamp_dato) %>%
  
  # --- Historiske resultater ---
  mutate(
    seneste_kamp = lag(vff_vundet), # Resultat af forrige hjemmekamp
    vff_vundet_2 = lag(vff_vundet, 1) == "vundet" & 
      lag(vff_vundet, 2) == "vundet" # 2 sejre i træk før denne kamp
  ) %>%
  
  # --- Variabelvalg ---
  # Behold relevante kolonner til analyse 
  dplyr::select(
    sæson, kamp_dato, ugedag, tid, runde, kamp, stilling, tilskuere, dommer,
    tv_kanal, tidsgruppe, seneste_kamp, vff_vundet_2, år
  )

# ---- Integrer API data ind i superstatsdata i RStudio (Vi gør det også i SQL) ----
vff_kampdata <- vff_kampdata_upload %>% 
  # --- Helligdage ---
  # Indsættelse af helligdag data fra date.nager.at og ser om datoen matcher kamp_dato
  mutate(helligdag = kamp_dato %in% as.Date(alle_helligdage$date)) %>%
  # --- Befolkningsdata ---
  # join med befolkningsdata
  mutate(
    år_char = as.character(år),
    kvartal = paste0("K", quarter(kamp_dato))
  ) %>%
  filter(!is.na(kamp_dato)) %>%
  
  # Join kvartalsbaseret befolkningsdata
  left_join(viborg_befolkning_komplet, by = c("år_char" = "år", "kvartal")) %>%
  # Join årlig befolkningsdata som backup
  left_join(
    viborg_befolkning_komplet %>%
      filter(is.na(kvartal)) %>%
      select(år, Indbyggere_Viborg_Kommune) %>%
      rename(Indbyggere_årlig = Indbyggere_Viborg_Kommune),
    by = c("år_char" = "år")
  ) %>%
  # Fyld manglende kvartalsdata med årsdata 
  mutate(
    Indbyggere_Viborg_Kommune = coalesce(Indbyggere_Viborg_Kommune, Indbyggere_årlig)
  ) %>%
  
  # Akkumuleret befolkning
  # Beregn vækst i indbyggertal 
  arrange(kamp_dato) %>%
  mutate(
    basis_befolkning = first(Indbyggere_Viborg_Kommune), #baseline
    akk_indbyggertal = Indbyggere_Viborg_Kommune - basis_befolkning #vækst
  ) %>%
  # --- Variabelvalg ---
  # Behold relevante kolonner til analyse 
  dplyr::select(
    sæson, kamp_dato, ugedag, tid, runde, kamp, stilling, tilskuere, dommer,
    tv_kanal, helligdag, kvartal, Indbyggere_Viborg_Kommune,
    akk_indbyggertal, tidsgruppe, seneste_kamp, vff_vundet_2
  )


# ---- DATALAGRING ---- #
# Gem RDS
saveRDS(vff_kampdata, "data/vff_kampdata.rds")

# Load RDS
vff_kampdata <- readRDS("data/vff_kampdata.rds")

# ---- SE RESULTAT ---- #
view(vff_kampdata)

# ---- HENT DMI DATA ---- #
# 1. Læs kampdatoer
datoer <- as.Date(vff_kampdata$kamp_dato, format = "%d-%m-%Y")

# 2. API-parametre
base_url <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?"
stationId <- "stationId=06065"  # års Syd
readRenviron("~/.Renviron")
apikey <- paste0("api-key=", Sys.getenv("DMI_API_KEY"))

# 3. Samlet dataframe
all_data <- data.frame()

extract_properties <- function(f) {
  # Case 1: GeoJSON format
  if (is.list(f) && "properties" %in% names(f) && is.list(f$properties)) {
    return(as.data.frame(f$properties))
  }
  
  # Case 2: Already a flat list
  if (is.list(f) && !"geometry" %in% names(f)) {
    return(as.data.frame(f))
  }
  
  return(NULL)
}

# 4. Hent data for hver kampdato (7 dage før)
for (date in datoer) {
  kamp_dato <- as.Date(date)
  start_date <- kamp_dato - 7
  end_date <- kamp_dato
  
  datetime <- paste0("&datetime=", start_date, "T00:00:00Z/", end_date, "T23:59:59Z")
  url <- paste0(base_url, stationId, "&", datetime, "&", apikey)
  
  message("Henter data fra: ", url)
  
  res <- GET(URLencode(url))
  json_txt <- rawToChar(res$content)
  
  if (nchar(json_txt) > 0) {
    data <- fromJSON(json_txt)
    
    if (!is.null(data$features) && length(data$features) > 0) {
      
      df <- map_df(data$features, extract_properties)
      df$kamp_dato <- kamp_dato
      
      all_data <- bind_rows(all_data, df)
    }
  }
}

# Tjek om den har hentet data
names(all_data)

# 5. Klargør data til wide format
wide_data <- all_data %>%
  select(kamp_dato, observed, parameterId, value) %>%
  mutate(observed = as.Date(observed)) %>%
  filter(parameterId %in% c("precip_past1h", "temp_mean_past1h", "wind_gust_always_past1h")) %>%
  group_by(kamp_dato, observed, parameterId) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = parameterId, values_from = value) %>%
  rename(regn = precip_past1h, temperatur = temp_mean_past1h, vind = wind_gust_always_past1h) %>%
  arrange(kamp_dato, observed)

#### gem og load RDS
# saveRDS(vff_kampdata_upload, "data/vff_kampdata_upload.rds")
# saveRDS(alle_helligdage, "data/alle_helligdage.rds")
# saveRDS(viborg_befolkning_komplet, "data/viborg_befolkning_komplet.rds")
# saveRDS(wide_data, "data/wide_data.rds")

vff_kampdata_upload <- readRDS("data/vff_kampdata_upload.rds")
alle_helligdage <- readRDS("data/alle_helligdage.rds")
viborg_befolkning_komplet <- readRDS("data/viborg_befolkning_komplet.rds")
wide_data <- readRDS("data/wide_data.rds")
vffkort01 <- readRDS("data/vffkort01.rds")

# Inspekt data
glimpse(wide_data)
glimpse(viborg_befolkning_komplet)
glimpse(alle_helligdage)
glimpse(vff_kampdata_upload)
glimpse(vffkort01)

# Giv vffkort01 kamp_dato variable
vffkort01 <- vffkort01 %>%
  left_join(
    vff_kampdata_upload %>%
      select(år, tilskuere, kamp_dato),
    by = c("år", "tilskuere")
  )
view(vffkort01)

# Ændre format i viborg_befolkning_komplet og alle_helligdage
viborg_befolkning_komplet$år <- as.double(viborg_befolkning_komplet$år)
alle_helligdage$date <- as.Date(alle_helligdage$date)

# Giv viborg_befolkning_komplet kamp_dato variable
viborg_befolkning_komplet <- viborg_befolkning_komplet %>% 
  select(år, Indbyggere_Viborg_Kommune) %>% 
  group_by(år) %>% 
  slice_max(Indbyggere_Viborg_Kommune, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  left_join(
    vff_kampdata_upload %>% select(år, kamp_dato),
    by = "år"
  )

# Join i SQLLite
con <- dbConnect(RSQLite::SQLite(), "data/VFF_data.sqlite")

uploadtables <- list(
  vff_kampdata_upload = vff_kampdata_upload,
  alle_helligdage = alle_helligdage,
  viborg_befolkning_komplet = viborg_befolkning_komplet,
  wide_data = wide_data,
  vffkort01 = vffkort01
)

for(name in names(uploadtables)) {
  cat("Uploading table:", name, "\n")
  dbWriteTable(con, name, uploadtables[[name]], overwrite = TRUE, row.names = FALSE)
}

dbListTables(con, schema = "dbo")

# dbExecute(con, "DROP TABLE kombineret_runde_table")

# Your SQL query as a string
query <- "SELECT 
              v.*,
              w.regn,
              w.temperatur,
              w.vind,
              h.localName AS helligdag,
              b.Indbyggere_Viborg_Kommune,
              k.d10_tilskuere,
              k.d7_tilskuere,
              k.d3_tilskuere
          FROM vff_kampdata_upload AS v
          
          -- Join wide_data på kamp_dato
          LEFT JOIN wide_data AS w
              ON v.kamp_dato = w.kamp_dato
          
          -- Join alle_helligdage på date
          LEFT JOIN alle_helligdage AS h
              ON v.kamp_dato = h.date
          
          -- Join viborg_befolkning_komplet på år
          LEFT JOIN viborg_befolkning_komplet AS b
              ON v.kamp_dato = b.kamp_dato
          
          -- Join vffkort01 på kamp_dato
          LEFT JOIN vffkort01 AS k
              ON v.kamp_dato = k.kamp_dato
              
          -- WHERE fjerner værdier under 0
          WHERE v.tilskuere > 0
          
          -- Hvis flere rækker kamp_dato, behold kun 1
          GROUP BY v.kamp_dato
          
          -- Behold grupper hvor der findes 1 kamp på den dato
          HAVING COUNT(v.kamp_dato) = 1;
          "

# Run the query and get the result as a data frame in R
joined_data <- dbGetQuery(con, query) %>%
  mutate(kamp_dato = as.Date(kamp_dato))

saveRDS(joined_data, "data/joined_data.rds")

dbDisconnect(con)

