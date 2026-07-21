# ============================================
# Linear Model 2
# House Price vs Drug Offence Rate
# ============================================

library(readr)
library(dplyr)
library(ggplot2)

# Load datasets
house_prices <- read_csv("Clean Data/house_prices_all_years.csv")
crime <- read_csv("Clean Data/crime_clean.csv")
population <- read_csv("Clean Data/population_clean.csv")

# Average house price by county
house_summary <- house_prices %>%
  group_by(county) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

print(house_summary)

# Drug offences by county
drug_summary <- crime %>%
  filter(crime_type == "Drugs") %>%
  group_by(county) %>%
  summarise(
    drug_cases = n(),
    .groups = "drop"
  )

print(drug_summary)

# Calculate drug offence rate per 100,000 population
drug_rate <- drug_summary %>%
  left_join(population, by = "county") %>%
  mutate(
    drug_rate = (drug_cases / total_population) * 100000
  )

print(drug_rate)

# Combine datasets
linear_data <- house_summary %>%
  left_join(
    drug_rate %>%
      select(county, drug_rate),
    by = "county"
  )

print(linear_data)

# Linear regression
model <- lm(avg_price ~ drug_rate, data = linear_data)

summary(model)

# Scatter plot
plot <- ggplot(
  linear_data,
  aes(
    x = drug_rate,
    y = avg_price,
    colour = county
  )
) +
  geom_point(size = 4) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linetype = "dashed",
    aes(group = 1)
  ) +
  labs(
    title = "House Price vs Drug Offence Rate",
    x = "Drug Offence Rate (per 100,000 people)",
    y = "Average House Price (£)"
  ) +
  theme_minimal()

print(plot)

# Save chart
ggsave(
  "Charts/LM2_HousePrice_vs_DrugOffenceRate.png",
  plot = plot,
  width = 8,
  height = 6,
  dpi = 300
)