#==========================================
# EDA 11 - Attainment 8 Box Plot
#==========================================

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Read cleaned education data
education <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/education_clean.csv",
  show_col_types = FALSE
)

# Filter one academic year
education_2024 <- education %>%
  filter(year == "2024-2025")

# Summary statistics
education_2024 %>%
  group_by(county) %>%
  summarise(
    Mean = mean(att8_score, na.rm = TRUE),
    Median = median(att8_score, na.rm = TRUE),
    SD = sd(att8_score, na.rm = TRUE),
    Minimum = min(att8_score, na.rm = TRUE),
    Maximum = max(att8_score, na.rm = TRUE)
  ) %>%
  print()

# Create box plot
education_plot <- ggplot(
  education_2024,
  aes(
    x = county,
    y = att8_score,
    fill = county
  )
) +
  geom_boxplot() +
  labs(
    title = "Average Attainment 8 Scores (2024-2025)",
    x = "County",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()

# Display chart
print(education_plot)

# Create Charts folder
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Charts",
  showWarnings = FALSE
)

# Save chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/eda11_attainment8_boxplot.png",
  plot = education_plot,
  width = 8,
  height = 6
)