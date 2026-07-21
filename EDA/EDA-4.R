library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load broadband data
broadband <- read_csv("Clean Data/broadband_clean.csv")

# Extract postcode district (e.g. NR1, NR14)
broadband <- broadband %>%
  mutate(
    district = str_extract(postcode, "^[A-Z]{1,2}[0-9]{1,2}")
  )

# Norfolk only
norfolk <- broadband %>%
  filter(county == "NORFOLK")

# Keep districts with at least 5 records
district_counts <- norfolk %>%
  count(district)

top_districts <- district_counts %>%
  filter(n >= 5) %>%
  pull(district)

norfolk <- norfolk %>%
  filter(district %in% top_districts)

# Plot
ggplot(norfolk,
       aes(x = reorder(district, avg_download, median),
           y = avg_download)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Average Download Speed by Postcode District (Norfolk)",
    x = "Postcode District",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save
ggsave(
  "Charts/broadband_boxplot_norfolk.png",
  width = 10,
  height = 6,
  dpi = 300
)