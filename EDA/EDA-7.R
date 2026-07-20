#==========================================
# EDA 7 - Crime Box Plot (2024)
#==========================================

library(readr)
library(dplyr)
library(ggplot2)

# Read cleaned crime data
crime <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/crime_clean.csv",
  show_col_types = FALSE
)

# Keep only 2024 and one crime type
crime_box <- crime %>%
  filter(
    grepl("^2024", month),
    crime_type == "Violence and sexual offences"
  )

# Summary statistics
crime_summary <- crime_box %>%
  group_by(county) %>%
  summarise(
    Mean = mean(as.numeric(as.factor(district))),
    Median = median(as.numeric(as.factor(district))),
    Count = n()
  )

print(crime_summary)

# Box Plot
ggplot(crime_box,
       aes(x = county,
           y = as.numeric(as.factor(district)),
           fill = county)) +
  
  geom_boxplot(width = 0.6) +
  
  labs(
    title = "Violence and Sexual Offences (2024)",
    x = "County",
    y = "District Index"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Save chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/crime_boxplot_2024.png",
  width = 8,
  height = 6
)