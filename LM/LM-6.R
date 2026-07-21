#==========================================
# Linear Model 6
# Average Download Speed vs Average Attainment 8 Score
#==========================================

library(readr)
library(dplyr)
library(ggplot2)

# Read datasets
broadband <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/broadband_clean.csv",
  show_col_types = FALSE
)

education <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/education_clean.csv",
  show_col_types = FALSE
)

# Average download speed by county
broadband_summary <- broadband %>%
  group_by(county) %>%
  summarise(
    avg_download_speed = mean(avg_download, na.rm = TRUE),
    .groups = "drop"
  )

# Average Attainment 8 score by county
education_summary <- education %>%
  group_by(county) %>%
  summarise(
    avg_attainment8 = mean(att8_score, na.rm = TRUE),
    .groups = "drop"
  )

# Merge datasets
model6_data <- left_join(
  broadband_summary,
  education_summary,
  by = "county"
)

print(model6_data)

# Linear regression
model6 <- lm(
  avg_download_speed ~ avg_attainment8,
  data = model6_data
)

# Model summary
summary(model6)

# Scatter plot
plot6 <- ggplot(
  model6_data,
  aes(
    x = avg_attainment8,
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
    title = "Average Download Speed vs Average Attainment 8 Score",
    x = "Average Attainment 8 Score",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal()

print(plot6)

# Create Charts folder
dir.create(
  "C:/Users/Bibek/OneDrive/Documents/Charts",
  showWarnings = FALSE
)

# Save chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/lm6_download_vs_attainment.png",
  plot = plot6,
  width = 8,
  height = 6
)