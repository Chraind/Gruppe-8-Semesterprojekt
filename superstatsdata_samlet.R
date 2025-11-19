pacman::p_load(tidyverse, vroom, janitor, polite, rjstat, rvest, lubridate, stringi, httr, jsonlite, purrr, utils)

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
    col_names <- c("ugedag","dato","kamp","stilling","tilskuere","dommer","delete","tv_kanal","sæson")
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

# TODO: upload scraped data til SQL databasen

# Gem RDS så man ikke ddos'er superstats, da man kun behøver køre ovenstående kode når man vil have opdateret tabel
saveRDS(kombineret_runde_table, "alt_superstats_data.rds")

# Load RDS
alt_superstats_data <- readRDS("alt_superstats_data.rds")
view(alt_superstats_data)

# ---- date.nager.at data hentning ----
alle_helligdage <- tibble()
for (year in sæson_år) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", year, "/DK")
  response <- GET(url)
  helligdage <- fromJSON(content(response, "text", encoding = "UTF-8"))
  alle_helligdage <- bind_rows(alle_helligdage, helligdage)
  Sys.sleep(1)
}

# ---- HENT BEFOLKNINGSDATA FRA DANMARKS STATISTIK ----
# Definer URLs
urls <- list(
  for_2005 = "https://api.statbank.dk/v1/data/bef1a/JSONSTAT?OMRÅDE=791%2C761%2C763%2C769%2C775%2C789&Tid=2002%2C2003%2C2004",
  aar_2005_2007 = "https://api.statbank.dk/v1/data/bef1a07/JSONSTAT?OMRÅDE=791&Tid=2005%2C2006%2C2007",
  efter_2008 = "https://api.statbank.dk/v1/data/folk1a/JSONSTAT?OMRÅDE=791&KØN=TOT&ALDER=IALT&CIVILSTAND=TOT&Tid=2008K1%2C2008K2%2C2008K3%2C2008K4%2C2013K1%2C2013K2%2C2013K3%2C2013K4%2C2014K1%2C2014K2%2C2014K3%2C2014K4%2C2015K1%2C2015K2%2C2015K3%2C2015K4%2C2016K1%2C2016K2%2C2016K3%2C2016K4%2C2017K1%2C2017K2%2C2017K3%2C2017K4%2C2021K1%2C2021K2%2C2021K3%2C2021K4%2C2022K1%2C2022K2%2C2022K3%2C2022K4%2C2023K1%2C2023K2%2C2023K3%2C2023K4%2C2024K1%2C2024K2%2C2024K3%2C2024K4%2C2025K1%2C2025K2%2C2025K3%2C2025K4"
)

# Hent og bearbejd data for 2003-2004 (skal summeres da Kommunen var opdelt anderledes den gang)
stat_viborg_for_2005 <- fromJSONstat(urls$for_2005) %>%
  as_tibble() %>%
  pull(1) %>%
  group_by(tid) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  mutate(
    aar = as.character(tid),
    kvartal = as.character(NA)
  ) %>%
  dplyr::select(aar, kvartal, value)

# Hent og bearbejd data for 2005-2007
stat_viborg_2005_2007 <- fromJSONstat(urls$aar_2005_2007) %>%
  as_tibble() %>%
  pull(1) %>%
  dplyr::select(tid, value) %>%
  mutate(
    aar = as.character(tid),
    kvartal = as.character(NA)
  ) %>%
  dplyr::select(aar, kvartal, value)

# Hent og bearbejd data efter 2008 (nu med kvartals optælling)
stat_viborg_efter_2008 <- fromJSONstat(urls$efter_2008) %>%
  as_tibble() %>%
  pull(1) %>%
  dplyr::select(tid, value) %>%
  mutate(
    aar = str_extract(tid, "^\\d{4}"),
    kvartal = str_extract(tid, "K\\d")
  ) %>%
  dplyr::select(aar, kvartal, value)

# Kombiner datasæt til én samlet tibble
viborg_befolkning_komplet <- bind_rows(
  stat_viborg_for_2005,
  stat_viborg_2005_2007,
  stat_viborg_efter_2008
) %>%
  arrange(aar, kvartal) %>%
  rename(Indbyggere_Viborg_Kommune = value)


# ---- DATARENSNING ----
kampdata_vff <- alt_superstats_data %>%
  # Split dato_tid i separate kolonner
  separate(dato, into = c("dato", "tid"), sep = " ", remove = FALSE) %>%

  # VFF involveret i udekampe eller hjemmekampe
  mutate(
    vff_spiller = grepl("(VFF|Viborg)", kamp, ignore.case = TRUE)
  ) %>%
  
  # Filtrer de kampe hvor VFF spiller og hvor tilskuere eksisterer
  filter(vff_spiller == TRUE) %>%
  filter(!is.na(tilskuere)) %>% 
  
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
  
  # Tilføj kamp_dato
  mutate (
    kamp_dato = dmy(paste0(dato, "/", år))
  ) %>% 
  
  # Indsættelse af helligdag data fra date.nager.at og ser om datoen matcher kamp_dato
  mutate(
    helligdag = kamp_dato %in% as.Date(alle_helligdage$date)
  ) %>% 
  
  # Tilføj tids kolonne, tidligt midt sent
  mutate(
    # Formater tid så de står i timer, minutter og sekunder (HH:MM:SS)
    tid = ifelse(grepl("^\\d{2}:\\d{2}$", tid), paste0(tid, ":00"), tid),
    tid = na_if(tid, ""),  # hvis NA eksisterer brug "" i stedet
    
    klokkeslæt = hms::as_hms(tid),
    
    tidsgruppe = case_when(
      klokkeslæt >= hms("12:00:00") & klokkeslæt < hms("15:30:00") ~ "tidligt",
      klokkeslæt >= hms("15:30:00") & klokkeslæt < hms("18:30:00") ~ "midt",
      klokkeslæt >= hms("18:30:00") & klokkeslæt <= hms("23:59:59") ~ "sent",
      TRUE ~ NA_character_
    )
  ) %>% 
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
  
  # Sorter efter dato
  arrange(kamp_dato) %>%
  
  # Lag tidligere kampresultat
  mutate(
    vff_vundet_sidste_kamp = lag(vff_vundet),
    # New column: won last two matches
    # Tilføj ny kolonne med om de har vundet de sidste to kampe
    vff_vundet_sidste_to_kampe = case_when(
      lag(vff_vundet, 1) == "vundet" & lag(vff_vundet, 2) == "vundet" ~ "ja",
      TRUE ~ "nej"
    )
  ) %>%
  
  # Fjern unødvendige kolonner
  dplyr::select(-delete, -month, -end_year, -score_home, -score_away, -vff_score, -modstander_score)

# ---- Kun VFF Hjemmekampe ----
kampdata_vff_hjemmekampe <- kampdata_vff %>%
# Identificer VFF hjemmekampe
# Hjemmeholdet står først i "kamp" kolonnen (f.eks. "VFF-FCK" eller "Viborg FF - FCK")
mutate(
  # Tjek om VFF/Viborg er hjemmehold (står før bindestreg)
  vff_hjemme = grepl("^(VFF|Viborg)", kamp, ignore.case = TRUE)
) %>%
  
  # Filtrer kun VFF hjemmekampe og kampe hvor tilskuere er NA
  filter(vff_hjemme == TRUE) %>% 
  
  # Omorganiser kolonner
  dplyr::select(sæson, runde, år, ugedag, dato, tid, kamp, stilling, tilskuere, dommer, 
                tv_kanal, kamp_dato, helligdag, tidsgruppe, vff_vundet_sidste_kamp, vff_vundet_sidste_to_kampe)

# Gem RDS
saveRDS(kampdata_vff_hjemmekampe, "kampdata_vff_hjemmekampe.rds")

# Load RDS
kampdata_vff_hjemmekampe <- readRDS("kampdata_vff_hjemmekampe.rds")

view(kampdata_vff_hjemmekampe)

### Tilføj befolkningsdata til kampdata
vff_kampdata_med_befolkning <- kampdata_vff_hjemmekampe %>%
  mutate(
    # Beregn kvartal fra kamp_dato
    år_char = as.character(år),
    kvartal = paste0("K", quarter(kamp_dato))
  ) %>%
  # Fjern rækker uden gyldig dato
  filter(!is.na(kamp_dato)) %>%
  # Join med kvartalsvise data først
  left_join(viborg_befolkning_komplet, by = c("år_char" = "aar", "kvartal")) %>%
  # For år uden kvartalsdata (2003-2007), hent det årlige tal
  left_join(
    viborg_befolkning_komplet %>%
      filter(is.na(kvartal)) %>%
      dplyr::select(aar, Indbyggere_Viborg_Kommune) %>%
      rename(Indbyggere_aarlig = Indbyggere_Viborg_Kommune),
    by = c("år_char" = "aar")
  ) %>%
  # Brug kvartalsdata hvis det er der, ellers årlige data
  mutate(
    Indbyggere_Viborg_Kommune = coalesce(Indbyggere_Viborg_Kommune, Indbyggere_aarlig)
  ) %>%
  # Fjern hjælpekolonner
  dplyr::select(-år_char, -Indbyggere_aarlig) %>%
  # Sorter kronologisk
  arrange(kamp_dato) %>%
  # Tilføj akkumuleret befolkningsvækst
  mutate(
    # Find befolkningstal for første kamp i 2003
    basis_befolkning = first(Indbyggere_Viborg_Kommune),
    # Beregn akkumuleret vækst fra 2003
    akk_indbyggertal = Indbyggere_Viborg_Kommune - basis_befolkning
  ) %>%
  # Fjern basis_befolkning hjælpekolonne
  dplyr::select(-basis_befolkning) %>%
  # Omorganiser kolonner så befolkningsdata kommer sidst
  dplyr::select(sæson, år, ugedag, dato, tid, kamp, stilling, tilskuere, dommer, 
                tv_kanal, kamp_dato, helligdag, kvartal, Indbyggere_Viborg_Kommune, 
                akk_indbyggertal, tidsgruppe, vff_vundet_sidste_kamp, vff_vundet_sidste_to_kampe)

# Se resultatet
View(vff_kampdata_med_befolkning)

