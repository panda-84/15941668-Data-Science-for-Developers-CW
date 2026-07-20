#==========================================
# EDA 1 - House Price Box Plot (2025)
#==========================================

library(readr)
library(dplyr)
library(ggplot2)

# Create year column
house <- house %>%
  mutate(year = as.numeric(format(transfer_date, "%Y")))

# Filter only 2025
house_2025 <- house %>%
  filter(year == 2025)

# Summary statistics
house_summary <- house_2025 %>%
  group_by(county) %>%
  summarise(
    Mean = round(mean(price), 2),
    Median = round(median(price), 2),
    SD = round(sd(price), 2),
    Minimum = min(price),
    Maximum = max(price),
    Total_Houses = n()
  )

print(house_summary)

# Box Plot
ggplot(house_2025,
       aes(x = county,
           y = price,
           fill = county)) +
  
  geom_boxplot(width = 0.6) +
  
  labs(
    title = "House Price Distribution (2025)",
    x = "County",
    y = "House Price (£)"
  ) +
  
  theme_minimal()

# Save graph
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/house_price_boxplot_2025.png",
  width = 8,
  height = 6
)