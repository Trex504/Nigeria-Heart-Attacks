library(dplyr)
library(ggplot2)
library(readxl)
library(janitor)
library(tidyverse)
library(sf)
library(gganimate)
library(DescTools)

df <- read_csv("data/heart_attack.csv")
df <- clean_names(df)

age_cholesterol_data <- df %>%
  group_by(age) %>%
  summarize(avg_cholesterol = mean(cholesterol, na.rm = TRUE),
    heart_attack_rate = mean(heart_attack, na.rm = TRUE) * 100) %>%
  ungroup()

avg_heart_attack_rate <- mean(age_cholesterol_data$heart_attack_rate, na.rm = TRUE)

age_cholesterol_data <- age_cholesterol_data %>%
  mutate(is_above_avg = heart_attack_rate > avg_heart_attack_rate)

ggplot(age_cholesterol_data, aes(x = age, y = avg_cholesterol)) +
  geom_point(aes(color = heart_attack_rate, shape = is_above_avg, size = is_above_avg), 
             fill = "white") +
  geom_smooth(se = FALSE, color = "purple", size = 1.5) +
  scale_color_gradient(low = "blue", high = "red", name = "Heart Attack Rate") +
  scale_shape_manual(values = c(16, 8)) +  # 16 = circle, 8 = star
  scale_size_manual(values = c(3, 5)) +  # 3 = smaller, 5 = bigger
  labs(title = "Cholesterol Levels by Age with Heart Attack Rate",
    x = "Age", y = "Average Cholesterol Level",
    caption = "Source: Heart Attack Dataset") +
  theme_dark() + 
  theme(axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 16, face = "bold", color = "black"),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),)

