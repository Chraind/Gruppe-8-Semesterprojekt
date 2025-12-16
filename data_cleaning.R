pacman::p_load(tidyverse, lubridate)

# Indlæs data fra data_gathering.R script
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
    uge = isoweek(kamp_dato),
    ferie_navn = case_when(
      uge == 7 ~ "vinterferie",
      uge %in% 26:32 ~ "sommerferie",
      uge == 42 ~ "efterårsferie",
      uge %in% 52:1 & month(kamp_dato) %in% c(12, 1) ~ "juleferie",
      helligdag == 1 ~ "helligdag",
      TRUE ~ NA_character_
    ),
    
    ferie_flag = factor(
      ifelse(!is.na(ferie_navn), 1, 0),
      levels = c(0, 1),
      labels = c("nej", "ja")
    )
  )

# Tilføj salg_3, salg_7, salg_10 kolonner ud fra d3_tilskuere, d7_tilskuere, d10_tilskuere til brug i ML model
joined_data <- joined_data %>% 
  mutate(
    salg_10 = d10_tilskuere,                # billetter solgt 10 dage før
    salg_7  = d7_tilskuere - d10_tilskuere, # yderligere billetter solgt mellem dag 10 og 7
    salg_3  = d3_tilskuere - d7_tilskuere,  # yderligere billetter solgt mellem dag 7 og 3
  )

# Gem renset data
saveRDS(joined_data, "data/joined_data_clean.rds")
