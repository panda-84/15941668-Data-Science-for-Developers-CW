install.packages(c(
  "DBI",
  "RSQLite",
  "readr",
  "dplyr"
))
library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

house <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/house_prices_all_years.csv",
  show_col_types = FALSE
)

broadband <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/broadband_clean.csv",
  show_col_types = FALSE
)

crime <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/crime_clean.csv",
  show_col_types = FALSE
)

education <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/education_clean.csv",
  show_col_types = FALSE
)

population <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/population_clean.csv",
  show_col_types = FALSE
)


county <- data.frame(
  
  county_id = c(1,2),
  
  county_name = c(
    "NORFOLK",
    "SUFFOLK"
  )
  
)

house <- house %>%
  left_join(
    county,
    by = c("county" = "county_name")
  ) %>%
  select(
    price,
    town = town_city,
    county_id,
    year
  )

names(house)
head(house)

house <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/house_prices_all_years.csv"
)

house <- house %>%
  mutate(year = as.integer(substr(transfer_date, 1, 4)))

house <- house %>%
  left_join(county,
            by = c("county" = "county_name")) %>%
  select(
    price,
    town = town_city,
    county_id,
    year
  )

library(DBI)
library(RSQLite)

con <- dbConnect(
  SQLite(),
  "C:/Users/Bibek/OneDrive/Documents/property_database.db"
)

dbWriteTable(
  con,
  "County",
  county,
  overwrite = TRUE
)

dbWriteTable(
  con,
  "HousePrices",
  house,
  overwrite = TRUE
)

dbWriteTable(
  con,
  "Broadband",
  broadband,
  overwrite = TRUE
)

dbWriteTable(
  con,
  "Crime",
  crime,
  overwrite = TRUE
)

dbWriteTable(
  con,
  "Education",
  education,
  overwrite = TRUE
)

dbWriteTable(
  con,
  "Population",
  population,
  overwrite = TRUE
)

dbListTables(con)
dbReadTable(con, "HousePrices")
dbGetQuery(con, "SELECT * FROM Broadband LIMIT 10")
dbGetQuery(con, "SELECT * FROM Crime LIMIT 10")
dbGetQuery(con, "SELECT * FROM Education LIMIT 10")
dbGetQuery(con, "SELECT * FROM Population LIMIT 10")

dbListFields(con, "HousePrices")

dbListFields(con, "Broadband")

dbListFields(con, "Crime")

dbListFields(con, "Education")

dbListFields(con, "Population")

dbDisconnect(con)
