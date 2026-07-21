# ============================================
# EDA 9
# Labelled Pie Chart of Robbery Rate
# October 2024
# ============================================

library(readr)
library(dplyr)

# ============================================
# Load datasets
# ============================================

crime <- read_csv("Clean Data/crime_clean.csv")
population <- read_csv("Clean Data/population_clean.csv")

# ============================================
# Filter robbery crimes for October 2024
# ============================================

robbery_data <- crime %>%
  filter(
    month == "2024-10",
    crime_type == "Robbery"
  )

# Check records
print(head(robbery_data))

# ============================================
# Count robberies by district
# ============================================

district_robbery <- robbery_data %>%
  count(
    county,
    district,
    name = "robbery_cases"
  )

print(district_robbery)

# ============================================
# Population for each district
# ============================================

district_population <- tibble(
  
  district = c(
    "Breckland",
    "Broadland",
    "Great Yarmouth",
    "King's Lynn and West Norfolk",
    "North Norfolk",
    "Norwich",
    "South Norfolk",
    "Babergh",
    "East Suffolk",
    "Ipswich",
    "Mid Suffolk",
    "West Suffolk"
  ),
  
  population = c(
    145700,
    129000,
    99000,
    152800,
    103900,
    144000,
    140300,
    92700,
    250000,
    139000,
    102500,
    180000
  )
  
)

# ============================================
# Calculate robbery rate
# ============================================

district_robbery <- district_robbery %>%
  left_join(
    district_population,
    by = "district"
  ) %>%
  mutate(
    robbery_rate = round(
      (robbery_cases / population) * 100000,
      2
    )
  ) %>%
  filter(!is.na(robbery_rate))

print(district_robbery)

# ============================================
# Labels
# ============================================

pie_labels <- paste0(
  district_robbery$district,
  "\n",
  district_robbery$robbery_rate
)

# ============================================
# Draw Pie Chart
# ============================================

pie(
  district_robbery$robbery_rate,
  labels = pie_labels,
  main = "Robbery Rate per 100,000 Population by District\n(October 2024)",
  col = terrain.colors(nrow(district_robbery)),
  clockwise = TRUE
)

# ============================================
# Save Pie Chart
# ============================================

png(
  "Charts/EDA9_Robbery_PieChart.png",
  width = 900,
  height = 700
)

pie(
  district_robbery$robbery_rate,
  labels = pie_labels,
  main = "Robbery Rate per 100,000 Population by District\n(October 2024)",
  col = terrain.colors(nrow(district_robbery)),
  clockwise = TRUE
)

dev.off()

cat("EDA 9 completed successfully!\n")