pacman::p_load(tidyverse, ggthemes, ggridges,janitor, vroom, arrow, readxl, writexl,
               stringi, hms, tidymodels, MASS, tseries, ggfortify, polite, tufte, rvest)

min_url <- "https://superstats.dk/program?season="
sæson_url <- "2026"

for (year in 2003:2026) {
  sæson_url <- as.character(year)
  full_url <- paste0(min_url, sæson_url)
}

#bjarnes kode
tables01 <- min_url %>% 
  read_html(encoding = "UTF-8") %>% 
  html_nodes("table") 

length(tables01)

tables01a <- html_table(tables01[[33]]) 

html_table(tables01, convert = FALSE)

?html_table








