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
# vff_vundet_2 som lgl
joined_data <- joined_data %>%
  mutate(
    vff_vundet_2 = vff_vundet_2 == 1
  )

# Helligdag som logisk faktor
joined_data <- joined_data %>%
  mutate(
    helligdag = !is.na(helligdag)
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


