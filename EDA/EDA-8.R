# ============================================
# EDA 8
# Radar Chart - Vehicle Crime Rate per 100,000
# May 2023 to April 2024
# ============================================

library(readr)
library(dplyr)
library(tidyr)
library(fmsb)

# ============================================
# STEP 1: Load datasets
# ============================================

crime <- read_csv("Clean Data/crime_clean.csv")
population <- read_csv("Clean Data/population_clean.csv")

# ============================================
# STEP 2: Filter Vehicle Crime
# (May 2023 - April 2024)
# ============================================

vehicle_crime <- crime %>%
  filter(
    crime_type == "Vehicle crime",
    month >= "2023-05",
    month <= "2024-04"
  )

# Check records
print(head(vehicle_crime))

# ============================================
# STEP 3: Count vehicle crimes
# ============================================

monthly_vehicle <- vehicle_crime %>%
  group_by(county, month) %>%
  summarise(
    vehicle_cases = n(),
    .groups = "drop"
  )

print(monthly_vehicle)

# ============================================
# STEP 4: Calculate crime rate
# per 100,000 population
# ============================================

monthly_vehicle <- monthly_vehicle %>%
  left_join(population, by = "county") %>%
  mutate(
    vehicle_rate = round(
      (vehicle_cases / total_population) * 100000,
      2
    )
  )

print(monthly_vehicle)

# ============================================
# STEP 5: Convert to radar format
# ============================================

radar_table <- monthly_vehicle %>%
  select(
    county,
    month,
    vehicle_rate
  ) %>%
  pivot_wider(
    names_from = month,
    values_from = vehicle_rate,
    values_fill = 0
  ) %>%
  arrange(county)

print(radar_table)

# ============================================
# STEP 6: Prepare radar dataframe
# ============================================

radar_data <- as.data.frame(radar_table[, -1])

rownames(radar_data) <- radar_table$county

max_value <- ceiling(max(radar_data) * 1.2)
min_value <- 0

radar_data <- rbind(
  rep(max_value, ncol(radar_data)),
  rep(min_value, ncol(radar_data)),
  radar_data
)

rownames(radar_data)[1] <- "Maximum"
rownames(radar_data)[2] <- "Minimum"

# ============================================
# STEP 7: Display Radar Chart
# ============================================

radarchart(
  radar_data,
  axistype = 1,
  pcol = c("steelblue", "darkorange"),
  pfcol = c(
    rgb(0.27,0.51,0.71,0.30),
    rgb(1,0.55,0,0.30)
  ),
  plwd = 3,
  cglcol = "grey70",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.8,
  title = "Vehicle Crime Rate per 100,000 Population\n(May 2023 - April 2024)"
)

legend(
  "topright",
  legend = c("Norfolk", "Suffolk"),
  col = c("steelblue", "darkorange"),
  lwd = 3,
  bty = "n"
)

# ============================================
# STEP 8: Save Radar Chart
# ============================================

png(
  "Charts/EDA8_Vehicle_Crime_Radar.png",
  width = 900,
  height = 900,
  res = 150
)

radarchart(
  radar_data,
  axistype = 1,
  pcol = c("steelblue", "darkorange"),
  pfcol = c(
    rgb(0.27,0.51,0.71,0.30),
    rgb(1,0.55,0,0.30)
  ),
  plwd = 3,
  cglcol = "grey70",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.8,
  title = "Vehicle Crime Rate per 100,000 Population\n(May 2023 - April 2024)"
)

legend(
  "topright",
  legend = c("Norfolk", "Suffolk"),
  col = c("steelblue", "darkorange"),
  lwd = 3,
  bty = "n"
)

dev.off()

cat("EDA 8 completed successfully!\n")