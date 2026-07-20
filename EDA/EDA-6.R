#==========================================
# EDA 6 - Average vs Maximum Download Speed
#==========================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read cleaned broadband dataset
broadband <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/broadband_clean.csv",
  show_col_types = FALSE
)

# Calculate county averages
speed_summary <- broadband %>%
  group_by(county) %>%
  summarise(
    Average_Download = mean(avg_download),
    Maximum_Download = mean(max_download)
  )

print(speed_summary)

# Convert to long format
speed_long <- speed_summary %>%
  pivot_longer(
    cols = c(Average_Download, Maximum_Download),
    names_to = "Speed_Type",
    values_to = "Speed"
  )

# Bar Chart
ggplot(speed_long,
       aes(x = county,
           y = Speed,
           fill = Speed_Type)) +
  
  geom_col(position="dodge") +
  
  labs(
    title="Average vs Maximum Download Speed",
    x="County",
    y="Speed (Mbps)",
    fill="Type"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title=element_text(face="bold",hjust=0.5)
  )

# Save chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/download_speed_bar_chart.png",
  width=8,
  height=6
)