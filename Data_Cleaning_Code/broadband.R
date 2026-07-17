library(readr)
library(dplyr)
library(tidyr)
# Read the dataset
broadband <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/internet/201805_fixed_pc_performance_r03.csv"
)

# Inspect
glimpse(broadband)
summary(broadband)

# Remove duplicate rows
broadband <- broadband %>%
  distinct()

# Check missing values
colSums(is.na(broadband))

broadband <- broadband %>%
  select(
    postcode,
    postcode_area,
    average_download,
    maximum_download,
    average_upload,
    maximum_upload
  )
install.packages("janitor")   # Only if you don't have it
library(janitor)

broadband <- broadband %>%
  clean_names()
names(broadband)

broadband <- broadband %>%
  select(
    postcode,
    postcode_area,
    average_download_speed_mbit_s,
    maximum_download_speed_mbit_s,
    average_upload_speed_mbit_s,
    maximum_upload_speed_mbit_s
  )

colSums(is.na(broadband))

broadband <- broadband %>%
  drop_na()

broadband <- broadband %>%
  distinct()


broadband <- broadband %>%
  filter(
    average_download_speed_mbit_s > 0,
    maximum_download_speed_mbit_s > 0,
    average_upload_speed_mbit_s > 0,
    maximum_upload_speed_mbit_s > 0
  )

summary(broadband)

str(broadband)

head(broadband)


# Create folder (only once)
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/broadband_clean",
  showWarnings = FALSE
)
write_csv(
  broadband,
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/broadband_clean/broadband_clean.csv"
)