#==========================================
# Education Data Cleaning (2021-2025)
#==========================================

# Load libraries
library(readr)
library(dplyr)
library(stringr)

#------------------------------------------
# Locate all education files
#------------------------------------------

ks4_files <- list.files(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/education",
  pattern = "ks4final.csv$",
  recursive = TRUE,
  full.names = TRUE
)

school_files <- list.files(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/education",
  pattern = "school_information.csv$",
  recursive = TRUE,
  full.names = TRUE
)

#------------------------------------------
# Read all KS4 files
#------------------------------------------

ks4_data <- lapply(ks4_files, function(file){
  
  read_csv(file, col_types = cols(.default = "c")) %>%
    mutate(
      year = str_extract(file, "[0-9]{4}-[0-9]{4}")
    ) %>%
    select(
      URN,
      LEA,
      ATT8SCR,
      year
    )
  
}) %>%
  bind_rows()

#------------------------------------------
# Read all School Information files
#------------------------------------------

school_data <- lapply(school_files, function(file){
  
  read_csv(file, col_types = cols(.default = "c")) %>%
    select(
      URN,
      TOWN
    )
  
}) %>%
  bind_rows() %>%
  distinct(URN, .keep_all = TRUE)

#------------------------------------------
# Clean and merge
#------------------------------------------

education_clean <- ks4_data %>%
  mutate(
    
    county = case_when(
      LEA == "926" ~ "NORFOLK",
      LEA == "935" ~ "SUFFOLK",
      TRUE ~ NA_character_
    ),
    
    att8_score = as.numeric(ATT8SCR)
    
  ) %>%
  
  inner_join(
    school_data,
    by = "URN"
  ) %>%
  
  filter(!is.na(att8_score)) %>%
  
  distinct(
    URN,
    year,
    .keep_all = TRUE
  ) %>%
  
  rename(
    town = TOWN
  ) %>%
  
  select(
    URN,
    town,
    county,
    year,
    att8_score
  )

#------------------------------------------
# Validation
#------------------------------------------

cat("Total Records :", nrow(education_clean), "\n")

head(education_clean)

table(
  education_clean$county,
  education_clean$year
)

education_clean %>%
  count(URN, year) %>%
  filter(n > 1)

#------------------------------------------
# Save cleaned dataset
#------------------------------------------

dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data",
  showWarnings = FALSE
)

write_csv(
  education_clean,
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/education_clean.csv"
)

cat("\nEducation dataset cleaned successfully.\n")