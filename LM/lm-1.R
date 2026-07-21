# ============================================
# Linear Model 1
# House Price vs Average Download Speed
# ============================================

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load datasets
house_prices <- read_csv("Clean Data/house_prices_all_years.csv")
broadband <- read_csv("Clean Data/broadband_clean.csv")

# Extract year from transfer date
house_prices <- house_prices %>%
  mutate(
    year = year(transfer_date)
  )

# Average house price by county and year
house_summary <- house_prices %>%
  group_by(county, year) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

print(house_summary)

# Average broadband speed by county
broadband_summary <- broadband %>%
  group_by(county) %>%
  summarise(
    avg_download = mean(avg_download, na.rm = TRUE),
    .groups = "drop"
  )

print(broadband_summary)

# Combine datasets
linear_data <- house_summary %>%
  left_join(broadband_summary, by = "county")

print(linear_data)

# Linear regression model
model <- lm(avg_price ~ avg_download, data = linear_data)

summary(model)

# Scatter plot
plot <- ggplot(
  linear_data,
  aes(
    x = avg_download,
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
    title = "House Price vs Average Download Speed",
    x = "Average Download Speed (Mbps)",
    y = "Average House Price (£)"
  ) +
  theme_minimal()

print(plot)

# Save chart
ggsave(
  "Charts/LM1_HousePrice_vs_DownloadSpeed.png",
  plot = plot,
  width = 8,
  height = 6,
  dpi = 300
)