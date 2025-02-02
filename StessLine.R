library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(janitor)
library(tidyverse)
library(sf)
library(gganimate)
library(DescTools)

df <- read_csv("data/heart_attack.csv")
df <- clean_names(df)

agg_data <- df %>%
  group_by(stress_level, heart_attack) %>%
  summarize(count = n(), .groups = "drop")

agg_data <- agg_data %>%
  mutate(original_count = count, 
         count = ifelse(heart_attack == FALSE, count * 0.1, count))

ggplot(agg_data, aes(x = stress_level, y = count, color = factor(heart_attack))) +
  geom_point(size = 5) + 
  geom_line(data = subset(agg_data, heart_attack == TRUE), aes(group = heart_attack), color = "red", size = 1.2) +  
  geom_line(data = subset(agg_data, heart_attack == FALSE), aes(group = heart_attack), color = "gray", linetype = "dashed") +  
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "green")) +  
  geom_text(
    data = agg_data, 
    aes(label = original_count), 
    vjust = -1, hjust = 0.5, size = 3, color = "white"  
  ) +
  labs(
    title = "Count of Heart Attack Cases by Stress Level",
    x = "Stress Level",
    y = "Count of Heart Attack Cases",
    color = "Heart Attack Status"
  ) +
  theme_dark() +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "top",
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.2)   
  )

