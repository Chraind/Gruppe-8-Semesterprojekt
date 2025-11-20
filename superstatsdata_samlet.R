pacman::p_load(tidyverse, vroom, janitor, polite, rjstat, rvest, lubridate, stringi, httr, jsonlite, purrr, utils)

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
          tilskuere = as.numeric(gsub("[^0-9]", "", tilskuere))
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


######
#####
###
# TODO: upload scraped data til SQL databasen
###
####
#####



# ---- HENT DATA FRA DATE.NAGER.AT ---- #
alle_helligdage <- tibble()
for (year in sæson_år) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", year, "/DK")
  response <- GET(url)
  helligdage <- fromJSON(content(response, "text", encoding = "UTF-8"))
  alle_helligdage <- bind_rows(alle_helligdage, helligdage)
  Sys.sleep(1)
}

# ---- HENT BEFOLKNINGSDATA FRA DANMARKS STATISTIK ---- #
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


# ---- DATARENSNING ---- #
vff_kampdata <- kombineret_runde_table %>%
  
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
  
  # --- Helligdage ---
  # Indsættelse af helligdag data fra date.nager.at og ser om datoen matcher kamp_dato
  mutate(helligdag = kamp_dato %in% as.Date(alle_helligdage$date)) %>%
  
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
  
  # --- Befolkningsdata ---
  # join med befolkningsdata
  mutate(
    år_char = as.character(år),
    kvartal = paste0("K", quarter(kamp_dato))
  ) %>%
  filter(!is.na(kamp_dato)) %>%
  
  # Join kvartalsbaseret befolkningsdata
  left_join(viborg_befolkning_komplet, by = c("år_char" = "aar", "kvartal")) %>%
  # Join årlig befolkningsdata som backup
  left_join(
    viborg_befolkning_komplet %>%
      filter(is.na(kvartal)) %>%
      select(aar, Indbyggere_Viborg_Kommune) %>%
      rename(Indbyggere_aarlig = Indbyggere_Viborg_Kommune),
    by = c("år_char" = "aar")
  ) %>%
  # Fyld manglende kvartalsdata med årsdata 
  mutate(
    Indbyggere_Viborg_Kommune = coalesce(Indbyggere_Viborg_Kommune, Indbyggere_aarlig)
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
