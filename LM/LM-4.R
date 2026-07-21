#==========================================
# Linear Model 4
# Average Attainment 8 Score vs Drug Offence Rate
#==========================================

library(readr)
library(dplyr)
library(ggplot2)

# Read datasets
education <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/education_clean.csv",
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

# Average Attainment 8 score by county
education_summary <- education %>%
  group_by(county) %>%
  summarise(
    avg_attainment8 = mean(att8_score, na.rm = TRUE),
    .groups = "drop"
  )

# Drug offences by county
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
model4_data <- left_join(
  education_summary,
  drug_summary,
  by = "county"
)

print(model4_data)

# Linear regression
model4 <- lm(
  avg_attainment8 ~ drug_rate,
  data = model4_data
)

# Regression summary
summary(model4)

# Scatter plot
plot4 <- ggplot(
  model4_data,
  aes(
    x = drug_rate,
    y = avg_attainment8
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
    title = "Average Attainment 8 Score vs Drug Offence Rate",
    x = "Drug Offence Rate (per 100,000 population)",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal()

print(plot4)

# Save chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/lm4_attainment_vs_drug.png",
  plot = plot4,
  width = 8,
  height = 6
)