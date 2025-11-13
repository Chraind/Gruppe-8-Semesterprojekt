pacman::p_load(tidyverse, ggthemes, ggridges,janitor, vroom, arrow, readxl, writexl,
               stringi, hms, tidymodels, MASS, tseries, ggfortify, polite, tufte, rvest, dplyr, purrr)

#min_url <- "https://superstats.dk/program?season="
#sæson_url <- "2026"

#for (year in 2003:2026) {
 # sæson_url <- as.character(year)
  #full_url <- paste0(min_url, sæson_url)
#}

#bjarnes kode
#tables01 <- min_url %>% 
 # read_html(encoding = "UTF-8") %>% 
  #html_nodes("table") 

#length(tables01)

#tables01a <- html_table(tables01[[33]]) 

#html_table(tables01, convert = FALSE)

#?html_table


main_url <- "https://superstats.dk/program?season="#link til superstats.dk
seasons <- 2003:2026# fra 2002/2003 til 2025/2026 seasonår
vff_data <- list()#Der bliver lavvet en list over data

#for vert year in years altså for vært år i år bliver der hentet tablerne fra superstats 




  
  

for (season in seasons) {
  url <- paste0(main_url, season)
  page <- read_html(url, encoding = "UTF-8")
  tables <- html_nodes(page, "table")
  
  kamp_table <- lapply(tables, html_table, convert = TRUE)
  
  contains_vff <- any(grepl("VFF", unlist(kamp_table), ignore.case = TRUE))
  
  if (contains_vff) {
    vff_data[[as.character(season)]] <- kamp_table
    message("Viborg FF fundet i ", season)
  }
}

 
  
  
  combined <- imap_dfr(vff_data, function(matches, season) {
    map_dfr(matches, ~ {
      if (is.data.frame(.x)) {
        # Tilføj fallback-navne og gør dem unikke
        bad_names <- is.na(names(.x)) | names(.x) == ""
        if (any(bad_names)) {
          names(.x)[bad_names] <- paste0("col", seq_along(.x))[bad_names]
        }
        names(.x) <- make.unique(names(.x))
        
        .x %>%
          mutate(across(everything(), as.character)) %>%
          janitor::clean_names() %>%
          mutate(year = as.numeric(season))  # Tilføj år
      } else {
        tibble()
      }
    })
  })
  names(combined)




final_data <- combined %>%
    mutate(
      # Dag (fx Lør, Søn)
      dag = pmap_chr(across(matches("^runde_\\d+$")), ~ first(na.omit(c(...)))),
      
      # Dato og tid (fx 27/07 17:00)
      dato_tid = pmap_chr(across(matches("^runde_\\d+_1$")), ~ first(na.omit(c(...)))),
      
      # Kamp, resultat, tilskuertal, dommer
      kamp = coalesce(col3, ""),
      resultat = coalesce(col4, ""),
      tilskuertal = coalesce(col5, ""),
      dommer = coalesce(col6, "")
    ) %>%
    separate(dato_tid, into = c("dato", "tid"), sep = " ", fill = "right", extra = "merge") %>%
    mutate(
      # Tilføj år til dato-strengen
      day_month = str_split(dato, "/", simplify = TRUE),
      month_num = as.numeric(day_month[,2]),
      full_dato = if_else(month_num %in% 7:12,
                          paste(dato, year - 1),
                          paste(dato, year)),
      kamp_dato = suppressWarnings(parse_date_time(full_dato, orders = c("dmy", "d.m.Y", "d/m/Y", "d-m-Y"))),
      
      # Kalenderår for kampen
      år = year(kamp_dato),
      
      
      # Sæsonnavn
      season = paste0(year - 1, "/", year),
      
          ) %>%
    dplyr::select(season, år, dag, dato, tid, kamp, resultat, tilskuertal, dommer,kamp_dato)



final_data_filtered <- final_data %>%
  filter(kamp_dato <= as.Date("09-11-2025", format = "%d-%m-%Y"))


vff_kampe <- final_data_filtered %>%
  filter(str_detect(kamp, "VFF")) %>%
  mutate(hjemme = str_detect(kamp, "^VFF")) %>%
  arrange(kamp_dato)

vff_hjemme <- vff_kampe %>% filter(hjemme)

