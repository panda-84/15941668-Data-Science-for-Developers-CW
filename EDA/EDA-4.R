#==========================================
# EDA 4 - Norfolk Broadband Box Plot
#==========================================

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Read cleaned broadband dataset
broadband <- read_csv(
  "C:/Users/Bibek/OneDrive/Documents/Clean Data/broadband_clean.csv",
  show_col_types = FALSE
)

# Keep only Norfolk
norfolk_speed <- broadband %>%
  filter(county == "NORFOLK")

# Summary statistics
norfolk_summary <- norfolk_speed %>%
  summarise(
    Mean = round(mean(avg_download),2),
    Median = round(median(avg_download),2),
    SD = round(sd(avg_download),2),
    Minimum = min(avg_download),
    Maximum = max(avg_download),
    Total = n()
  )

print(norfolk_summary)

# Box Plot
ggplot(norfolk_speed,
       aes(x = county,
           y = avg_download,
           fill = county)) +
  
  geom_boxplot(width = 0.5) +
  
  labs(
    title = "Average Download Speed Distribution - Norfolk",
    x = "",
    y = "Average Download Speed (Mbps)"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    plot.title = element_text(face="bold",hjust=0.5)
  )

# Save chart
ggsave(
  "C:/Users/Bibek/OneDrive/Documents/Charts/norfolk_download_boxplot.png",
  width=8,
  height=6
)