#==========================================
# Crime Data Cleaning
#==========================================

# Load libraries
library(readr)
library(dplyr)
library(stringr)

#------------------------------------------
# Locate all crime files
#------------------------------------------

crime_paths <- list.files(
  path = "C:/Users/Bibek/OneDrive/Documents/Raw_data/crime",
  pattern = "street\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Crime files found:", length(crime_paths), "\n")

#------------------------------------------
# Import all monthly datasets
#------------------------------------------

crime_data <- lapply(crime_paths, function(file){
  
  read_csv(
    file,
    show_col_types = FALSE
  )
  
}) %>%
  bind_rows()

#------------------------------------------
# Keep required variables
#------------------------------------------

crime_data <- crime_data %>%
  transmute(
    month = Month,
    lsoa = `LSOA name`,
    crime_type = `Crime type`
  )

#------------------------------------------
# Create district and county
#------------------------------------------

crime_clean <- crime_data %>%
  mutate(
    
    district = str_remove(lsoa, " [0-9A-Z]+$"),
    
    county = case_when(
      
      district %in% c(
        "Breckland",
        "Broadland",
        "Great Yarmouth",
        "King's Lynn and West Norfolk",
        "North Norfolk",
        "Norwich",
        "South Norfolk"
      ) ~ "NORFOLK",
      
      TRUE ~ "SUFFOLK"
      
    )
    
  ) %>%
  
  select(
    month,
    district,
    county,
    crime_type
  ) %>%
  
  distinct()

#------------------------------------------
# Validation
#------------------------------------------

cat("\nTotal Crime Records:", nrow(crime_clean), "\n\n")

head(crime_clean)

table(crime_clean$county)

table(crime_clean$crime_type)

#------------------------------------------
# Export cleaned dataset
#------------------------------------------

dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data",
  showWarnings = FALSE
)

write_csv(
  crime_clean,
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/crime_clean.csv"
)

cat("\nCrime dataset cleaned successfully.\n")

list.files("C:/Users/Bibek/OneDrive/Documents/Clean Data")