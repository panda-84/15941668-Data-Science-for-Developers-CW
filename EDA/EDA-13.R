#==========================================
# EDA 13 - Suffolk Attainment 8 Line Chart
#==========================================

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Read cleaned education dataset
education <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/education_clean.csv",
  show_col_types = FALSE
)

# Suffolk only
suffolk <- education %>%
  filter(county == "SUFFOLK")

# Average score by year and town
suffolk_summary <- suffolk %>%
  group_by(year, town) %>%
  summarise(
    avg_att8 = mean(att8_score, na.rm = TRUE),
    .groups = "drop"
  )

print(suffolk_summary)

# Create line chart
suffolk_plot <- ggplot(
  suffolk_summary,
  aes(
    x = year,
    y = avg_att8,
    colour = town,
    group = town
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Attainment 8 Score in Suffolk (2021–2025)",
    x = "Academic Year",
    y = "Average Attainment 8 Score",
    colour = "Town"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display chart
print(suffolk_plot)

# Create Charts folder
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Charts",
  showWarnings = FALSE
)

# Save chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/eda13_suffolk_attainment_line.png",
  plot = suffolk_plot,
  width = 10,
  height = 6
)