pacman::p_load(tidyverse, vroom, janitor, polite, rjstat, rvest, lubridate,
               stringi, httr, jsonlite, purrr, utils, RSQLite, DBI)

joined_data <- readRDS("data/joined_data.rds")

view(joined_data)
glimpse(joined_data)

joined_data <- joined_data %>%
  mutate(
    # Ugedag som faktor (bevar evt. rækkefølgen)
    ugedag = factor(ugedag, 
                    levels = c("Man", "Tir", "Ons", "Tor", "Fre", "Lør", "Søn"),
                    ordered = TRUE),
    
    # Tidsgruppe som faktor
    tidsgruppe = factor(tidsgruppe, 
                        levels = c("tidligt", "midt", "sent"), 
                        ordered = FALSE),
    
    # Seneste kamp som faktor
    seneste_kamp = factor(seneste_kamp,
                          levels = c("vundet", "uafgjort", "tabt")),
    
    # Regn: inddelt i "ingen", "lidt", "meget"
    regn_gruppe = case_when(
      is.na(regn)        ~ NA_character_,
      regn == 0          ~ "ingen regn",
      regn > 0 & regn <= 1 ~ "lidt regn",
      regn > 1           ~ "meget regn"
    ) %>% factor(levels = c("ingen regn", "lidt regn", "meget regn"))
  )

# Helligdag som logisk faktor
joined_data <- joined_data %>%
  mutate(
    helligdag = ifelse(is.na(helligdag), 0, 1)
  )

# Runde som double
joined_data <- joined_data %>%
  mutate(
    runde = as.numeric(gsub("Runde ", "", runde))
  )


joined_data <- joined_data %>%
  mutate(
    # Find befolkningstal for første kamp i 2003
    basis_befolkning = first(Indbyggere_Viborg_Kommune),
    # Beregn akkumuleret vækst fra 2003
    akk_indbyggertal = Indbyggere_Viborg_Kommune - basis_befolkning
  ) %>%
    # Fjern basis_befolkning hjælpekolonne
    dplyr::select(-basis_befolkning)

# hold i grupper
matchup_attendance <- joined_data %>%
  group_by(kamp) %>%
  summarise(
    avg_tilskuere = mean(tilskuere, na.rm = TRUE),
    n_kampe = n()
  ) %>%
  arrange(desc(avg_tilskuere))

view(matchup_attendance)

# TODO: få inddelt holdene i grupper per gennemsnitlige seere
# joined_data <- joined_data %>%
#   # remove spaces from kamp first
#   mutate(kamp = str_replace_all(kamp, " ", ""),
#          # assign kamp_gruppe based on kamp names
#          kamp_gruppe = case_when(
#            kamp %in% c("VFF-BIF","VFF-FCK","VFF-AGF","VFF-HIF","VFF-FCM","VFF-FCF","VFF-AaB") ~ "stor",
#            kamp %in% c("VFF-RFC","VFF-VB","VFF-SIF","VFF-LBK","VFF-FCV","VFF-OB","VFF-FCN") ~ "middel",
#            kamp %in% c("VFF-SJF","VFF-HOB","VFF-HER","VFF-EFB","VFF-ACH","VFF-BKF","VFF-KBK","VFF-AB") ~ "lille",
#            TRUE ~ NA_character_  # in case there are any other kamp names
#          )) %>%
#   mutate(kamp_gruppe = factor(kamp_gruppe, levels = c("lille","middel","stor")))

# TODO: lav en gruppe til sommerferie, vinterferie, sæt den sammen med helligdag

saveRDS(joined_data, "data/joined_data.rds")

