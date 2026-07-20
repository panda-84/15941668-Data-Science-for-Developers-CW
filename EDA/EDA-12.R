#==========================================
# EDA 12 - Norfolk Attainment 8 Line Chart
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

# Norfolk only
norfolk <- education %>%
  filter(county == "NORFOLK")

# Average score by year and town
norfolk_summary <- norfolk %>%
  group_by(year, town) %>%
  summarise(
    avg_att8 = mean(att8_score, na.rm = TRUE),
    .groups = "drop"
  )

print(norfolk_summary)

# Create line chart
norfolk_plot <- ggplot(
  norfolk_summary,
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
    title = "Average Attainment 8 Score in Norfolk (2021–2025)",
    x = "Academic Year",
    y = "Average Attainment 8 Score",
    colour = "Town"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display
print(norfolk_plot)

# Create Charts folder
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Charts",
  showWarnings = FALSE
)

# Save chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/eda12_norfolk_attainment_line.png",
  plot = norfolk_plot,
  width = 10,
  height = 6
)