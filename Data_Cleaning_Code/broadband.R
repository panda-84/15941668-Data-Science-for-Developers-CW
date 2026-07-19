#==========================================
# Broadband Data Cleaning
#==========================================

# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(janitor)

# Read the broadband dataset
internet_data <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/internet/201805_fixed_pc_performance_r03.csv",
  show_col_types = FALSE
)

# Convert column names to snake_case
internet_data <- internet_data %>%
  clean_names()

# Keep only required variables
broadband_clean <- internet_data %>%
  transmute(
    postcode,
    postcode_area,
    avg_download = average_download_speed_mbit_s,
    max_download = maximum_download_speed_mbit_s
  ) %>%
  filter(postcode_area %in% c("NR", "IP")) %>%
  mutate(
    county = case_when(
      postcode_area == "NR" ~ "NORFOLK",
      postcode_area == "IP" ~ "SUFFOLK"
    )
  ) %>%
  distinct() %>%
  drop_na()

# Check cleaned dataset
cat("Total Records:", nrow(broadband_clean), "\n")

head(broadband_clean)

summary(broadband_clean)

# Create output folder if it doesn't exist
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/broadband_clean",
  showWarnings = FALSE,
  recursive = TRUE
)

# Save cleaned dataset
write_csv(
  broadband_clean,
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/broadband_clean/broadband_clean.csv"
)

cat("Broadband cleaning completed successfully!\n")