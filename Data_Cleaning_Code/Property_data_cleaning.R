install.packages(c(
  "readr",
  "dplyr",
  "ggplot2",
  "tidyr",
  "DBI",
  "RSQLite",
  "corrplot"
))

# Load libraries
library(readr)
library(dplyr)


# Read the dataset
house2021 <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/property_price/pp-2021.csv",
  col_names = FALSE
)

# Add column names
colnames(house2021) <- c(
  "transaction_id",
  "price",
  "transfer_date",
  "postcode",
  "property_type",
  "old_new",
  "duration",
  "paon",
  "saon",
  "street",
  "locality",
  "town_city",
  "district",
  "county",
  "ppd_category",
  "record_status"
)

# Check the structure
glimpse(house2021)

# Check missing values
colSums(is.na(house2021))

# Remove duplicate rows
house2021 <- distinct(house2021)

# Keep only the useful columns
house2021 <- house2021 %>%
  select(
    price,
    transfer_date,
    postcode,
    property_type,
    town_city,
    district,
    county
  )

# Keep only Norfolk and Suffolk
house2021 <- house2021 %>%
  filter(county %in% c("NORFOLK", "SUFFOLK"))

# View cleaned data
head(house2021)

# Create folder (only once)
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price",
  showWarnings = FALSE
)

# Save cleaned file
write_csv(
  house2021,
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_2021_clean.csv"
)

clean_house_data <- function(input_file, output_file) {
  
  house <- read_csv(input_file, col_names = FALSE)
  
  colnames(house) <- c(
    "transaction_id",
    "price",
    "transfer_date",
    "postcode",
    "property_type",
    "old_new",
    "duration",
    "paon",
    "saon",
    "street",
    "locality",
    "town_city",
    "district",
    "county",
    "ppd_category",
    "record_status"
  )
  
  house <- house %>%
    distinct() %>%
    select(
      price,
      transfer_date,
      postcode,
      property_type,
      town_city,
      district,
      county
    ) %>%
    filter(county %in% c("NORFOLK", "SUFFOLK"))
  
  write_csv(house, output_file)
}


clean_house_data(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/property_price/pp-2022.csv",
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_2022_clean.csv"
)

clean_house_data(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/property_price/pp-2023.csv",
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_2023_clean.csv"
)

clean_house_data(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/property_price/pp-2024.csv",
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_2024_clean.csv"
)

clean_house_data(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/property_price/pp-2025.csv",
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_2025_clean.csv"
)

# Combine All Cleaned House Price Files
#==========================================

house_2021 <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_2021_clean.csv",
  show_col_types = FALSE
)

house_2022 <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_2022_clean.csv",
  show_col_types = FALSE
)

house_2023 <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_2023_clean.csv",
  show_col_types = FALSE
)

house_2024 <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_2024_clean.csv",
  show_col_types = FALSE
)

house_2025 <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_2025_clean.csv",
  show_col_types = FALSE
)

# Combine all datasets
house_prices_all <- bind_rows(
  house_2021,
  house_2022,
  house_2023,
  house_2024,
  house_2025
)

# Preview combined data
head(house_prices_all)

# Check total records
nrow(house_prices_all)

# Save combined dataset
write_csv(
  house_prices_all,
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_prices_all_years.csv"
)

cat("All house price files combined successfully!\n")

