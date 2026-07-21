# ============================================
# EDA 5
# Box Plot of Average Download Speed
# by Postcode District - Suffolk
# ============================================

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load cleaned broadband data
broadband <- read_csv("Clean Data/broadband_clean.csv")

# Extract postcode district (e.g. IP1, IP4, IP14)
broadband <- broadband %>%
  mutate(
    district = str_extract(postcode, "^[A-Z]{1,2}[0-9]{1,2}")
  )

# Filter Suffolk data
suffolk_broadband <- broadband %>%
  filter(county == "SUFFOLK")

# Count observations in each district
district_counts <- suffolk_broadband %>%
  count(district) %>%
  arrange(desc(n))

print(district_counts)

# Keep districts with at least 5 observations
top_districts <- district_counts %>%
  filter(n >= 5) %>%
  pull(district)

suffolk_filtered <- suffolk_broadband %>%
  filter(district %in% top_districts)

# Create box plot
box_plot <- ggplot(
  suffolk_filtered,
  aes(
    x = reorder(district, avg_download, median),
    y = avg_download
  )
) +
  geom_boxplot(
    fill = "darkorange",
    outlier.alpha = 0.3
  ) +
  labs(
    title = "Average Download Speed by Postcode District - Suffolk",
    x = "Postcode District",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(box_plot)

# Save chart
ggsave(
  "Charts/EDA5_Suffolk_Broadband_Boxplot.png",
  plot = box_plot,
  width = 10,
  height = 6,
  dpi = 300
)