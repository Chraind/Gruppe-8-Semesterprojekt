pacman::p_load(tidyverse, lubridate)

# Indlæs data
joined_data <- readRDS("data/joined_data.rds")

# Rens og transformer data
joined_data <- joined_data %>%
  mutate(
    # Ugedag som ordnet faktor
    ugedag = factor(ugedag, 
                    levels = c("Man", "Tir", "Ons", "Tor", "Fre", "Lør", "Søn"),
                    ordered = TRUE),
    
    # Tidsgruppe som faktor
    tidsgruppe = factor(tidsgruppe, levels = c("tidligt", "midt", "sent")),
    
    # Seneste kamp som faktor
    seneste_kamp = factor(seneste_kamp, levels = c("vundet", "uafgjort", "tabt")),
    
    # Regn grupperet i kategorier
    regn_gruppe = case_when(
      is.na(regn)        ~ NA_character_,
      regn == 0          ~ "ingen regn",
      regn > 0 & regn <= 1 ~ "lidt regn",
      regn > 1           ~ "meget regn"
    ) %>% factor(levels = c("ingen regn", "lidt regn", "meget regn")),
    
    # Helligdag flag
    helligdag = ifelse(is.na(helligdag), 0, 1),
    
    # Runde som numerisk
    runde = as.numeric(gsub("Runde ", "", runde))
  ) %>%
  # Beregn akkumuleret befolkningsvækst
  mutate(
    akk_indbyggertal = Indbyggere_Viborg_Kommune - first(Indbyggere_Viborg_Kommune)
  )

# Tilføj kamp_gruppe kolonne baseret på gennemsnitlige tilskuere for hver matchup
joined_data <- joined_data %>%
  group_by(kamp) %>%
  mutate(
    avg_tilskuere = mean(tilskuere, na.rm = TRUE),
    kamp_gruppe = case_when(
      avg_tilskuere <= 4000 ~ "lille",
      avg_tilskuere <= 5500 ~ "middel",
      TRUE ~ "stor"
    ) %>% factor(levels = c("lille", "middel", "stor"))
  ) %>%
  ungroup() %>%
  select(-avg_tilskuere)  # fjern hjælpekolonne

# Flag helligdage og skoleferier
joined_data <- joined_data %>%
  mutate(
    ferie_navn = case_when(
      between(kamp_dato, as.Date(paste0(år,"-02-11")), as.Date(paste0(år,"-02-17"))) ~ "vinterferie",
      between(kamp_dato, as.Date(paste0(år,"-04-15")), as.Date(paste0(år,"-04-21"))) ~ "påskeferie",
      between(kamp_dato, as.Date(paste0(år,"-06-25")), as.Date(paste0(år,"-08-10"))) ~ "sommerferie",
      between(kamp_dato, as.Date(paste0(år,"-10-14")), as.Date(paste0(år,"-10-20"))) ~ "efterårsferie",
      kamp_dato >= as.Date(paste0(år,"-12-23")) & kamp_dato <= as.Date(paste0(år+1,"-01-01")) ~ "juleferie",
      helligdag == 1 ~ "helligdag",
      TRUE ~ NA_character_
    ),
    # Lav ferie_flag om til faktor med niveauer "nej" og "ja"
    ferie_flag = factor(ifelse(!is.na(ferie_navn), 1, 0), levels = c(0, 1), labels = c("nej", "ja"))
  )

# Gem renset data
saveRDS(joined_data, "data/joined_data_clean.rds")
