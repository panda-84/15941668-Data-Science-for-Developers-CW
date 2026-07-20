#==========================================
# EDA 3 - Average House Price Line Chart
#==========================================

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Read cleaned house price dataset
house <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Raw_data/cleaned_property_price/house_prices_all_years.csv",
  show_col_types = FALSE
)

# Create year column if it does not exist
if(!"year" %in% names(house)){
  house <- house %>%
    mutate(year = as.numeric(format(as.Date(transfer_date), "%Y")))
}

# Calculate average house price by county and year
house_yearly <- house %>%
  group_by(year, county) %>%
  summarise(
    average_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

# Display summary
print(house_yearly)

# Create line chart
ggplot(house_yearly,
       aes(x = year,
           y = average_price,
           colour = county,
           group = county)) +
  
  geom_line(linewidth = 1.2) +
  
  geom_point(size = 3) +
  
  scale_x_continuous(
    breaks = 2021:2025
  ) +
  
  labs(
    title = "Average House Price Trend (2021–2025)",
    x = "Year",
    y = "Average House Price (£)",
    colour = "County"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Save chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/house_price_line_chart.png",
  width = 8,
  height = 6
)