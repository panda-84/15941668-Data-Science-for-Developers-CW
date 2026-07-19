#==========================================
# Population Data Cleaning
#==========================================

# Load required libraries
library(readr)
library(dplyr)
library(stringr)

# Read the population dataset
population_data <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/Population2011_1656567141570.csv",
  show_col_types = FALSE
)

# Clean and process the data
population_clean <- population_data %>%
  transmute(
    postcode = Postcode,
    population = as.numeric(gsub(",", "", Population)),
    postcode_area = str_extract(Postcode, "^[A-Z]+")
  ) %>%
  filter(postcode_area %in% c("NR", "IP")) %>%
  mutate(
    county = case_when(
      postcode_area == "NR" ~ "NORFOLK",
      postcode_area == "IP" ~ "SUFFOLK"
    )
  )

# Calculate total population by county
population_summary <- population_clean %>%
  group_by(county) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

# Preview
print(population_summary)

# Create output folder if it doesn't exist
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/population_clean",
  showWarnings = FALSE,
  recursive = TRUE
)

# Save cleaned dataset
write_csv(
  population_summary,
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/population_clean/population_clean.csv"
)

cat("Population dataset cleaned successfully!\n")