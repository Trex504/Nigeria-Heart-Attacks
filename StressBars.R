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

stress_factors_data <- df %>%
  pivot_longer(
    cols = c(region, occupation, income_level, education_level, marital_status), 
    names_to = "factor",
    values_to = "category"
  ) %>%
  group_by(factor, category) %>%
  summarize(avg_stress_level = mean(stress_level, na.rm = TRUE), .groups = "drop") %>%
  mutate(adjusted_stress_level = avg_stress_level - 5.35) 

ggplot(stress_factors_data, aes(x = category, y = adjusted_stress_level, fill = adjusted_stress_level)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~factor, scales = "free_x") +
  scale_fill_gradient(low = "blue", high = "red") + 
  labs(
    title = "Comparing Stress Levels with Personal Life Factors",
    x = "Factor Categories",
    y = "Stress Level",
    fill = "Stress Level"
  ) +
  theme_dark() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_line(color = "white", linetype = "solid"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

