#==========================================
# Linear Model 5
# Average Download Speed vs Drug Offence Rate
#==========================================

library(readr)
library(dplyr)
library(ggplot2)

# Read datasets
broadband <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/broadband_clean.csv",
  show_col_types = FALSE
)

crime <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/crime_clean.csv",
  show_col_types = FALSE
)

population <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/population_clean.csv",
  show_col_types = FALSE
)

# Average download speed by county
broadband_summary <- broadband %>%
  group_by(county) %>%
  summarise(
    avg_download_speed = mean(avg_download, na.rm = TRUE),
    .groups = "drop"
  )

# Drug offence rate by county
drug_summary <- crime %>%
  filter(crime_type == "Drugs") %>%
  group_by(county) %>%
  summarise(
    drug_cases = n(),
    .groups = "drop"
  ) %>%
  left_join(population, by = "county") %>%
  mutate(
    drug_rate = (drug_cases / total_population) * 100000
  )

# Merge datasets
model5_data <- left_join(
  broadband_summary,
  drug_summary,
  by = "county"
)

print(model5_data)

# Linear regression
model5 <- lm(
  avg_download_speed ~ drug_rate,
  data = model5_data
)

# Regression summary
summary(model5)

# Scatter plot
plot5 <- ggplot(
  model5_data,
  aes(
    x = drug_rate,
    y = avg_download_speed
  )
) +
  geom_point(size = 4, colour = "blue") +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "red"
  ) +
  geom_text(
    aes(label = county),
    vjust = -1
  ) +
  labs(
    title = "Average Download Speed vs Drug Offence Rate",
    x = "Drug Offence Rate (per 100,000 population)",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal()

print(plot5)

# Create Charts folder
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Charts",
  showWarnings = FALSE
)

# Save chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/lm5_download_vs_drug.png",
  plot = plot5,
  width = 8,
  height = 6
)