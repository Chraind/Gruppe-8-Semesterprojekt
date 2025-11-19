pacman::p_load(tidyverse, janitor, rvest, lubridate, stringi, httr, jsonlite, purrr)

fcidk <- readRDS("fcidk.rds")
vffkort01 <- readRDS("vffkort01.rds")
view(fcidk)
view(vffkort01)

# Install packages (run once)
install.packages("DBI")
install.packages("RSQLite")

# Load libraries
library(DBI)
library(RSQLite)

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "fodbolddata.sqlite")

# List all tables
dbListTables(con)

# Example: read a table (replace 'your_table' with an actual table name)
df <- dbReadTable(con, "db_fcidk")

# Or run a SQL query
result <- dbGetQuery(con, "SELECT * FROM db_fcidk")
resulta <- dbGetQuery(con, "SELECT * FROM db_vff")
view(result)
view(resulta)

# Disconnect when done
dbDisconnect(con)
