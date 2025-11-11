pacman::p_load(tidyverse, ggthemes, ggridges, janitor, vroom, arrow, readxl, writexl, purrr, 
               stringi, hms, tidymodels, MASS, tseries, ggfortify, polite, tufte, rvest, lubridate)
###
### Find de år hvor VFF er i superligaen
###

superliga_url <- "https://superstats.dk/hold/alltime?id=11"
read_superliga <- read_html(superliga_url, encoding = "UTF-8")

# superliga_table <- superliga_url %>% 
  #read_html(encoding = "UTF-8") %>% 
  #html_nodes("table")

# Læs den korrekte table, fundet ved at kigge på HTML siden
superliga_table_read <- read_superliga %>% 
  html_element("table#sortTable") %>% 
  html_table(fill = TRUE)

superliga_sæsoner <- superliga_table_read %>%
  mutate(
    sæson_år = str_extract(Sæson, "(?<=/)[0-9]{4}") %>% as.integer()
  )

sæson_år <- superliga_sæsoner %>% 
  filter(!is.na(sæson_år) & sæson_år >= 2003) %>% 
  pull(sæson_år)
#       sæson_år returnerer
#       [1] 2026 2025 2024 2023 2022 2017 2016 2014 2008 2007 2006 2005 2004 2003

# Finder alle kampe hvor VFF er i superligaen
min_url <- "https://superstats.dk/program?season="
sæson_urls <- c()

sæson_urls <- paste0(min_url, sæson_år)
sæson_urls

# Initialize empty data.frame
kombineret_runde_table <- tibble()

###
### MOTHER OF ALL FOR LOOPS
###

for (i in seq_along(sæson_urls)) {
  url <- sæson_urls[i]
  page <- read_html(url, encoding = "UTF-8")
  individuel_sæson <- sæson_år[i]
  
  # Extract all tables as HTML nodes
  table_nodes <- page %>% html_nodes("table")
  
  for (tbl_node in table_nodes) {
    df <- html_table(tbl_node, fill = TRUE)
    
    # Only keep tables with "Runde" in column names
    if (!any(grepl("Runde", names(df), ignore.case = TRUE))) next
    
    # TV info
    tv_titles <- tbl_node %>% html_nodes("td img[title]") %>% html_attr("title")
    if (length(tv_titles) == nrow(df)) df$tv <- tv_titles else df$tv <- NA_character_
    
    df$sæson <- individuel_sæson
    
    # Clean names & rename
    df <- df %>% clean_names()
    col_names <- c("ugedag","dato","matchup","stilling","tilskuere","judge","delete","tv_kanal","sæson")
    names(df)[1:min(length(col_names), ncol(df))] <- col_names[1:min(length(col_names), ncol(df))]
    
    # Fix Tilskuere
    if ("tilskuere" %in% names(df)) {
      df <- df %>%
        mutate(
          tilskuere = gsub("\\.","",tilskuere),
          tilskuere = gsub(",",".",tilskuere),
          tilskuere = as.numeric(tilskuere)
        )
    }
    
    # Parse date
    if ("dato" %in% names(df)) {
      df <- df %>%
        mutate(
          dato = paste0(dato,"/",individuel_sæson),
          dato = gsub("([0-9]{2}/[0-9]{2}) ([0-9]{2}:[0-9]{2})/([0-9]{4})","\\1/\\3 \\2",dato),
          dato = suppressWarnings(lubridate::dmy_hm(dato))
        )
    }
    
    # Remove unwanted columns (7 and 9) safely
    cols_to_remove <- intersect(c(7,9), seq_along(df))
    if (length(cols_to_remove) > 0) df <- df[,-cols_to_remove]
    
    # Append to main combined table
    kombineret_runde_table <- bind_rows(kombineret_runde_table, df)
  }
}

# Inspect
View(kombineret_runde_table)




































































###
### MOTHER OF ALL FOR LOOPS 
###

for (i in seq_along(sæson_urls)) {
  url <- sæson_urls[i]
  page <- read_html(url, encoding = "UTF-8")
  individuel_sæson <- sæson_år[i]
  
  # Extract all tables as HTML nodes
  table_nodes <- page %>% html_nodes("table")
  
  # Loop through tables
  tables <- lapply(table_nodes, function(tbl_node) {
    df <- html_table(tbl_node, fill = TRUE)  # Convert to tibble/data.frame
    
    # Only keep tables with "Runde" in column names
    if (!any(grepl("Runde", names(df), ignore.case = TRUE))) return(NULL)
    
    # Extract TV info from <img title="..."> inside this table
    tv_titles <- tbl_node %>%
      html_nodes("td img[title]") %>%  # look inside <td>
      html_attr("title")
    
    if (length(tv_titles) == nrow(df)) {
      df$TV <- tv_titles
    } else {
      df$TV <- NA_character_  # fallback if table has no TV icons
    }
    
    df$Sæson <- individuel_sæson  # Add Sæson column
    
    #Rens af navne og giv kolonne navn
    df <- df %>%  clean_names()
    col_names <- c("Ugedag", "Dato", "Matchup", "Stilling", "Tilskuere", "Judge", "delete", "Tv_kanal", "Sæson")
    names(df)[1:min(length(col_names), ncol(df))] <- col_names[1:min(length(col_names), ncol(df))]
    
    #Fix tilskuer parsing
    if ("Tilskuere" %in% names(df)) {
      df <- df %>%
        mutate(
          Tilskuere = gsub("\\.", "", Tilskuere),  # Remove thousands separator
          Tilskuere = gsub(",", ".", Tilskuere),   # Convert decimal comma to dot
          Tilskuere = as.numeric(Tilskuere)        # Convert to numeric
        )
    }
    
    # Put sæson dato i dato kolonne
    df <- df %>%
      mutate(
        Dato = paste0(Dato, "/", individuel_sæson),
        Dato = gsub("([0-9]{2}/[0-9]{2}) ([0-9]{2}:[0-9]{2})/([0-9]{4})", "\\1/\\3 \\2", Dato),
        Dato = suppressWarnings(dmy_hm(Dato))
      )
    df
  })
  
  # Remove NULL tables (tables without "Runde")
  tables <- tables[!sapply(tables, is.null)]
  
  #Indsæt i liste
  alle_sæson_tables[[as.character(individuel_sæson)]] <- tables
}


# Kombiner data
# Flatten the list of lists
all_tables_flat <- unlist(alle_sæson_tables, recursive = FALSE)

# Combine into one data.frame
kombineret_runde_table <- bind_rows(all_tables_flat)

# Remove columns 7 and 9 safely (if they exist)
cols_to_remove <- intersect(c(7, 9), seq_along(kombineret_runde_table))
kombineret_runde_table <- kombineret_runde_table[, -cols_to_remove]

# Inspect the result
view(kombineret_runde_table)


#Gem RDS
saveRDS(kombineret_runde_table, "superliga_raw.rds")
#Load RDS
kombineret_runde_table <- readRDS("superliga_raw.rds")


#kombiner alle rows
kombineret_runde_table <- bind_rows(unlist(alle_sæson_tables, recursive = FALSE))


glimpse(kombineret_runde_table)
view(kombineret_runde_table)



#hehe


#chatgpt
min_url <- "https://superstats.dk/program?season="

# Build sæson_urls (already filtered for 2003+)
sæson_urls <- paste0(min_url, sæson_år)
alle_sæson_tables <- list()

for (i in seq_along(sæson_urls)) {
  url <- sæson_urls[i]
  individuel_sæson <- sæson_år[i]
  
  page <- read_html(url, encoding = "UTF-8")
  tables <- page %>% html_nodes("table") %>% html_table(fill = TRUE)
  
  # Filter only "Runde" tables
  tables_with_runde <- tables %>% keep(~ any(grepl("Runde", names(.x), ignore.case = TRUE)))
  
  # Clean each table safely
  tables_with_runde <- lapply(tables_with_runde, function(tbl) {
    tbl <- suppressWarnings(janitor::clean_names(tbl))
    
    # Fix empty or duplicate names if any slipped through
    bad_names <- which(is.na(names(tbl)) | names(tbl) == "")
    if (length(bad_names) > 0) {
      names(tbl)[bad_names] <- paste0("x", bad_names)
    }
    
    # Limit to first 6 columns (your structure)
    tbl <- tbl[, 1:min(6, ncol(tbl))]
    
    # Rename consistently
    names(tbl) <- c("Ugedag", "Dato", "Matchup", "Stilling", "Tilskuere", "Judge")[1:ncol(tbl)]
    
    # Add season info and parse date
    tbl <- tbl %>%
      mutate(
        # Build full datetime string
        Dato = paste0(Dato, "/", individuel_sæson),
        # Reorder to dd/mm/yyyy hh:mm (so lubridate can parse)
        Dato = gsub("([0-9]{2}/[0-9]{2}) ([0-9]{2}:[0-9]{2})/([0-9]{4})", "\\1/\\3 \\2", Dato),
        # Parse safely
        Dato = suppressWarnings(lubridate::dmy_hm(Dato)),
        sæson_år = individuel_sæson
      )
    return(tbl)
  })
  
  alle_sæson_tables[[as.character(url)]] <- tables_with_runde
}

# Combine all tables
kombineret_runde_table <- bind_rows(unlist(alle_sæson_tables, recursive = FALSE))

# Preview result
head(kombineret_runde_table)
view(kombineret_runde_table)








#test

# Example single season URL
example_url <- "https://superstats.dk/program?season=2023"

# Read the page
page <- read_html(example_url, encoding = "UTF-8")

# Get all tables from this page
tables <- page %>% html_nodes("table") %>% html_table(fill = TRUE)

# Check how many tables
length(tables)

# Look at the first few tables to find the one you need
tables[[1]]   # first table
tables[[2]]   # second table
tables[[3]]   # etc.
tables[[33]]












#for (i in sæson_år) {
#  full_url <- paste0(min_url, i)
#  sæson_urls <- c(sæson_urls, full_url)
#}

#bjarnes kode
tables01 <- min_url %>% 
  read_html(encoding = "UTF-8") %>% 
  html_nodes("table") 

length(tables01)

tables01a <- html_table(tables01[[33]]) 

html_table(tables01, convert = FALSE)

?html_table








