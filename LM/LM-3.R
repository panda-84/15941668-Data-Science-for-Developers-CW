# ============================================
# Linear Model 3
# House Price vs Average Attainment 8 Scores
# ============================================

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load datasets
house_prices <- read_csv("Clean Data/house_prices_all_years.csv")
education <- read_csv("Clean Data/education_clean.csv")

# Create year column from transfer date
house_prices <- house_prices %>%
  mutate(
    year = year(as.Date(transfer_date))
  )

# Average house price per county per year
avg_price_year <- house_prices %>%
  group_by(county, year) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

print(avg_price_year)

# Average Attainment 8 score per county
attainment8 <- education %>%
  filter(year == "2024-2025") %>%
  group_by(county) %>%
  summarise(
    avg_att8 = mean(att8_score, na.rm = TRUE),
    .groups = "drop"
  )

print(attainment8)

# Join datasets
combined <- avg_price_year %>%
  left_join(attainment8, by = "county")

print(combined)

# Run the linear regression model
model <- lm(avg_price ~ avg_att8, data = combined)

summary(model)

# Create scatter plot with regression line
scatter_plot <- ggplot(
  combined,
  aes(
    x = avg_att8,
    y = avg_price,
    color = county
  )
) +
  geom_point(size = 4) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linetype = "dashed",
    aes(group = 1)
  ) +
  labs(
    title = "House Price vs Average Attainment 8 Scores",
    x = "Average Attainment 8 Score",
    y = "Average House Price (£)"
  ) +
  theme_minimal()

print(scatter_plot)

# Save the chart
ggsave(
  "Charts/lm_houseprice_vs_attainment8.png",
  scatter_plot,
  width = 8,
  height = 6,
  dpi = 300
)