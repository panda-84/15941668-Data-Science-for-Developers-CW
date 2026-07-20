#==========================================
# EDA 2 - Average House Price Bar Chart
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

# Check the data
glimpse(house)

# Create year column if it does not exist
if(!"year" %in% names(house)){
  house <- house %>%
    mutate(year = as.numeric(format(as.Date(transfer_date), "%Y")))
}

# Calculate average house price for each county
average_house_price <- house %>%
  group_by(county) %>%
  summarise(
    average_price = mean(price, na.rm = TRUE)
  )

# Display the summary
print(average_house_price)

# Create bar chart
ggplot(average_house_price,
       aes(x = county,
           y = average_price,
           fill = county)) +
  
  geom_col(width = 0.6) +
  
  geom_text(
    aes(label = paste0("£", round(average_price,0))),
    vjust = -0.4,
    size = 5
  ) +
  
  labs(
    title = "Average House Price in Norfolk and Suffolk (2021-2025)",
    x = "County",
    y = "Average House Price (£)"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

# Save the chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/average_house_price_bar.png",
  width = 8,
  height = 6
)