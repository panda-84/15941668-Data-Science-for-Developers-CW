#==========================================
# EDA 8 - Radar Chart of Crime Rates
# Norfolk vs Suffolk (April 2024)
#==========================================

# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(fmsb)

# Read cleaned datasets
crime <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/crime_clean.csv",
  show_col_types = FALSE
)

population <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/population_clean.csv",
  show_col_types = FALSE
)

# Select April 2024 and chosen crime types
crime_selected <- crime %>%
  filter(
    month == "2024-04",
    crime_type %in% c(
      "Vehicle crime",
      "Burglary",
      "Robbery",
      "Drugs",
      "Shoplifting"
    )
  )

# Count crimes
crime_summary <- crime_selected %>%
  group_by(county, crime_type) %>%
  summarise(
    crime_count = n(),
    .groups = "drop"
  )

# Join population
crime_summary <- crime_summary %>%
  left_join(population, by = "county")

# Calculate rate per 100000 population
crime_summary <- crime_summary %>%
  mutate(
    crime_rate = (crime_count / total_population) * 100000
  )

# Convert to wide format
radar_data <- crime_summary %>%
  select(county, crime_type, crime_rate) %>%
  pivot_wider(
    names_from = crime_type,
    values_from = crime_rate
  )

# Convert to dataframe
radar_df <- as.data.frame(radar_data)

# Set county names as row names
rownames(radar_df) <- radar_df$county
radar_df$county <- NULL

# Add max and min rows
max_values <- apply(radar_df, 2, max) * 1.2
min_values <- rep(0, ncol(radar_df))

radar_df <- rbind(
  max_values,
  min_values,
  radar_df
)

rownames(radar_df)[1] <- "Maximum"
rownames(radar_df)[2] <- "Minimum"

# Draw radar chart
radarchart(
  radar_df,
  axistype = 1,
  pcol = c(NA, NA, "blue", "red"),
  pfcol = c(
    NA,
    NA,
    rgb(0, 0, 1, 0.3),
    rgb(1, 0, 0, 0.3)
  ),
  plwd = 3,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  cglwd = 0.8,
  axislabcol = "black",
  vlcex = 0.9,
  title = "Crime Rate per 100,000 Population (April 2024)"
)

legend(
  "topright",
  legend = c("Norfolk", "Suffolk"),
  col = c("blue", "red"),
  lwd = 3,
  bty = "n"
)

# View calculated crime rates
print(crime_summary)


