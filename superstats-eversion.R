pacman::p_load(tidyverse, ggthemes, ggridges,janitor, vroom, arrow, readxl, writexl, stringi, hms, tidymodels, MASS, tseries, ggfortify, polite, tufte, rvest)

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
        ## fill = TRUE er godt når tabeller er lidt rodet, og man vil undgå fejl
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
        
        # Tilføjer år til tabellen
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

view(vff_kampdata)


# ---- DATARENSNING ----
vff_kampdata_clean <- vff_kampdata %>%
  # Split dato_tid i separate kolonner
  separate(dato_tid, into = c("dato", "tid"), sep = " ", remove = FALSE) %>%
  
  # Identificer VFF hjemmekampe
  # Hjemmeholdet står først i "kamp" kolonnen (f.eks. "VFF-FCK" eller "Viborg FF - FCK")
  mutate(
    # Tjek om VFF/Viborg er hjemmehold (står før bindestreg)
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

# Se resultatet
view(vff_kampdata_clean)
