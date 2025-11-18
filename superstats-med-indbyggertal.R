pacman::p_load(tidyverse, vroom, janitor, polite, httr, rjstat, jsonlite, lubridate, rvest, stringi, purrr, utils)

# === HENT BEFOLKNINGSDATA FRA DANMARKS STATISTIK ===
# Definer URLs
urls <- list(
  for_2005 = "https://api.statbank.dk/v1/data/bef1a/JSONSTAT?OMRÅDE=791%2C761%2C763%2C769%2C775%2C789&Tid=2003%2C2004",
  aar_2005_2007 = "https://api.statbank.dk/v1/data/bef1a07/JSONSTAT?OMRÅDE=791&Tid=2005%2C2006%2C2007",
  efter_2008 = "https://api.statbank.dk/v1/data/folk1a/JSONSTAT?OMRÅDE=791&KØN=TOT&ALDER=IALT&CIVILSTAND=TOT&Tid=2008K1%2C2008K2%2C2008K3%2C2008K4%2C2014K1%2C2014K2%2C2014K3%2C2014K4%2C2016K1%2C2016K2%2C2016K3%2C2016K4%2C2017K1%2C2017K2%2C2017K3%2C2017K4%2C2022K1%2C2022K2%2C2022K3%2C2022K4%2C2023K1%2C2023K2%2C2023K3%2C2023K4%2C2024K1%2C2024K2%2C2024K3%2C2024K4%2C2025K1%2C2025K2%2C2025K3%2C2025K4"
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


## Data fra Superstats 
# Base URL til Superstats
base_url_superstats <- "https://superstats.dk/program?season="

# Sæsoner VFF har været i SuperLigaen
viborg_superliga_years <- c(2003, 2004, 2005, 2006, 2007, 2008, 2014, 2016, 2017, 2022, 2023, 2024, 2025, 2026)

# tom liste til at gemme data
all_superstats_data <- list()

# Loop gennem alle sæsoner 
for (year in viborg_superliga_years) {
  #cat() printer beskeder til konsollen
  cat("Henter data for sæson", year, "...\n")
  
  #URL for denne sæson
  full_url <- paste0(base_url_superstats, year)
  
  # tilføjer en pause mellem requests (1) = 1 sek for at undgå overbelastning af serveren
  Sys.sleep(1)
  
  # Henter HTML tabeller 
  ## tryCatch er error handling - sker der en fejl kører den koden i error=function(e) i stedet
  ## for at forhindre hele scriptet crasher hvis en sæson skulle fejle 
  tryCatch({
    superstats_tables <- full_url %>%
      # Henter HTML-koden fra superstats
      read_html(encoding = "UTF-8") %>%
      # html_nodes("table") finder alle <table> tags på siden
      html_nodes("table") %>%
      # html_table() konverterer HTML tabeller til R dataframes og fill = TRUE fylder manglende celler med NA
        ## fill = TRUE er godt naar tabeller er lidt rodet, og man vil undgå fejl
      html_table(fill = TRUE)
    
    #Loop gennem hver tabel fra denne sæson
    for (i in seq_along(superstats_tables)) {
      tabel <- superstats_tables[[i]]
      
      # Tjekker om tabellen har den rigtige struktur for kampdata
      # Kampdata har typisk 6-8 kolonner og mindst 5 rækker
      if (ncol(tabel) >= 6 && nrow(tabel) >= 5) {
      
        # Konverter alt til character
        tabel <- as.data.frame(lapply(tabel, as.character), stringsAsFactors = FALSE)
        
        # Giver kolonnenavne
        if (ncol(tabel) == 7) {
          colnames(tabel) <- c("dag", "dato_tid", "kamp", "resultat", "tilskuere", "dommer", "tv_kanal")
        } else if (ncol(tabel) == 6) {
          colnames(tabel) <- c("dag", "dato_tid", "kamp", "resultat", "tilskuere", "dommer")
          tabel$tv_kanal <- NA
        } else if (ncol(tabel) == 8) {
          # Nogle tabeller har en ekstra kolonne - drop den første
          tabel <- tabel[, 2:8]
          colnames(tabel) <- c("dag", "dato_tid", "kamp", "resultat", "tilskuere", "dommer", "tv_kanal")
        } else {
          # Skip tabeller med uventet antal kolonner
          next
        }
        
        # Tilføjer aar til tabellen
        tabel$aar <- year
        
        # Gemmer kun hvis der er data tilbage
        # *all_superstats_data er en kasse med mapper. Hver gang der findes en tabel med kampdata, 
        # lægges der en ny mappe i kassen. Hvis mappen er tom (0 rækker), springes den over.
        if (nrow(tabel) > 0) {
          all_superstats_data <- c(all_superstats_data, list(tabel))
          cat("  Tabel", i, "-", nrow(tabel), "kampe\n")
        }
      }
    }
  }, error = function(e) {
    # Denne del kører kun hvis der sker en fejl
    cat(" Fejl ved hentning af data for", year, ":", e$message, "\n")  
  })
}

# Kombiner alle data til én dataframe
vff_kampdata <- bind_rows(all_superstats_data)

# Omrokerer kolonner - bruger dplyr::select for at undgå konflikt med MASS::select
vff_kampdata <- vff_kampdata %>%
  dplyr::select(aar, dato_tid, dag, kamp, resultat, tilskuere, dommer, tv_kanal)


# ---- DATARENSNING ----
vff_kampdata_clean <- vff_kampdata %>%
  # Split dato_tid i separate kolonner
  separate(dato_tid, into = c("dato", "tid"), sep = " ", remove = FALSE) %>%
  
  # Identificer VFF hjemmekampe
  # Hjemmeholdet staar først i "kamp" kolonnen (f.eks. "VFF-FCK" eller "Viborg FF - FCK")
  mutate(
    # Tjek om VFF/Viborg er hjemmehold (staar før bindestreg)
    vff_hjemme = grepl("^(VFF|Viborg)", kamp, ignore.case = TRUE),
    
    # bindestregen bruges som skilletegn - alt før bindestreg gemmes som hjemmehold 
    # og alt efter gemmes som udehold
    hjemmehold = str_trim(str_extract(kamp, "^[^-]+")),
    udehold = str_trim(str_extract(kamp, "[^-]+$"))
  ) %>%
  
  # Filtrer kun VFF hjemmekampe
  filter(vff_hjemme == TRUE) %>%
  
  # Konverter tilskuere til numerisk (fjern punktum-tusindtalsseparator hvis nødvendig)
  mutate(
    tilskuere = as.numeric(gsub("[^0-9]", "", tilskuere))
  ) %>%
  
  # Fjern unødvendige kolonner
  dplyr::select(-dag, -tv_kanal, -vff_hjemme, -kamp,) %>%
  
  # Omorganiser kolonner
  dplyr::select(aar, dato, tid, hjemmehold, udehold, resultat, tilskuere)

### Tilføj befolkningsdata til kampdata
vff_kampdata_med_befolkning <- vff_kampdata_clean %>%
  mutate(
    # Kombiner dato og aar til en fuld dato
    fuld_dato = dmy(paste(dato, aar)),
    # Beregn kvartal fra dato
    aar_char = as.character(aar),
    kvartal = paste0("K", quarter(fuld_dato))
  ) %>%
  # Fjern rækker uden gyldig dato
  filter(!is.na(fuld_dato)) %>%
  # Join med kvartalsvise data først
  left_join(viborg_befolkning_komplet, by = c("aar_char" = "aar", "kvartal")) %>%
  # For år uden kvartalsdata (2003-2007), hent det årlige tal
  left_join(
    viborg_befolkning_komplet %>%
      filter(is.na(kvartal)) %>%
      dplyr::select(aar, Indbyggere_Viborg_Kommune) %>%
      rename(Indbyggere_aarlig = Indbyggere_Viborg_Kommune),
    by = c("aar_char" = "aar")
  ) %>%
  # Brug kvartalsdata hvis tilgængelig, ellers årlige data
  mutate(
    Indbyggere_Viborg_Kommune = coalesce(Indbyggere_Viborg_Kommune, Indbyggere_aarlig)
  ) %>%
  # Fjern hjælpekolonner
  dplyr::select(-aar_char, -dato, -tid, -Indbyggere_aarlig) %>%
  # Omorganiser kolonner med fuld_dato først
  dplyr::select(fuld_dato, hjemmehold, udehold, resultat, tilskuere, kvartal, Indbyggere_Viborg_Kommune) %>%
  # Sorter kronologisk
  arrange(fuld_dato)%>%
  # Tilføj akkumuleret befolkningsvækst
  mutate(
    # Find befolkningstal for første kamp i 2003
    basis_befolkning = first(Indbyggere_Viborg_Kommune),
    # Beregn akkumuleret vækst fra 2003
    akk_indbyggertal = Indbyggere_Viborg_Kommune - basis_befolkning
  ) %>%
  # Fjern basis_befolkning hjælpekolonne
  dplyr::select(-basis_befolkning)

# Se resultatet
View(vff_kampdata_med_befolkning)
