#==========================================
# EDA 10 - Drug Offence Rate Line Chart
#==========================================

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Read cleaned datasets
crime <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/crime_clean.csv",
  show_col_types = FALSE
)

population <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/population_clean.csv",
  show_col_types = FALSE
)

# Filter only drug offences
drug <- crime %>%
  filter(crime_type == "Drugs")

# Extract year
drug <- drug %>%
  mutate(
    year = substr(month, 1, 4)
  )

# Count offences
drug_summary <- drug %>%
  group_by(year, county) %>%
  summarise(
    drug_cases = n(),
    .groups = "drop"
  )

# Join population
drug_summary <- drug_summary %>%
  left_join(population, by = "county")

# Calculate rate per 100000
drug_summary <- drug_summary %>%
  mutate(
    drug_rate = (drug_cases / total_population) * 100000
  )

print(drug_summary)

# Create Charts folder
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Charts",
  showWarnings = FALSE
)

# Create chart
drug_plot <- ggplot(
  drug_summary,
  aes(
    x = year,
    y = drug_rate,
    colour = county,
    group = county
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Drug Offence Rate per 100,000 Population (2023–2026)",
    x = "Year",
    y = "Drug Offence Rate",
    colour = "County"
  ) +
  theme_minimal()

# Display
print(drug_plot)

# Save
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/eda10_drug_offence_rate_line.png",
  plot = drug_plot,
  width = 8,
  height = 6
)