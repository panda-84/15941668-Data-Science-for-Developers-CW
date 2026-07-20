#==========================================
# EDA 9 - Robbery Rate Pie Chart
#==========================================

# Load libraries
library(readr)
library(dplyr)

# Read cleaned datasets
crime <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/crime_clean.csv",
  show_col_types = FALSE
)

population <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/population_clean.csv",
  show_col_types = FALSE
)

# Keep only April 2024 Robbery cases
robbery <- crime %>%
  filter(
    month == "2024-04",
    crime_type == "Robbery"
  )

# Count robberies by district
robbery_summary <- robbery %>%
  group_by(district, county) %>%
  summarise(
    robbery_cases = n(),
    .groups = "drop"
  )

# Join county population
robbery_summary <- robbery_summary %>%
  left_join(population, by = "county")

# Calculate robbery rate per 100000
robbery_summary <- robbery_summary %>%
  mutate(
    robbery_rate = (robbery_cases / total_population) * 100000
  )

# Create labels
labels <- paste0(
  robbery_summary$district,
  "\n",
  round(robbery_summary$robbery_rate,2)
)

# Create Charts folder if needed
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Charts",
  showWarnings = FALSE
)

# Save chart
png(
  "C:/Users/Bibek/OneDrive/Documents/Charts/eda9_robbery_pie_chart.png",
  width = 900,
  height = 700
)

pie(
  robbery_summary$robbery_rate,
  labels = labels,
  main = "Robbery Rate per 100,000 Population (April 2024)"
)

dev.off()

# Display chart in RStudio
pie(
  robbery_summary$robbery_rate,
  labels = labels,
  main = "Robbery Rate per 100,000 Population (April 2024)"
)

# View data
print(robbery_summary)