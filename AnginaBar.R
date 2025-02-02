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
df_counts <- df %>%
  group_by(angina, heart_attack) %>%
  summarise(count = n(), .groups = "drop")

# Create the bar graph with labels and customized gridlines
ggplot(df_counts, aes(
  x = factor(angina, labels = c("No Angina", "With Angina")), 
  y = count, 
  fill = factor(heart_attack, labels = c("No Heart Attack", "Heart Attack"))
)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_text(
    aes(label = count), 
    position = position_dodge(width = 0.8),
    vjust = -0.5,  # Position labels slightly above the bars
    size = 4, 
    color = "black"
  ) +
  scale_fill_manual(values = c("skyblue", "tomato"), name = "Heart Attack Status") +
  labs(
    title = "Comparison of Angina and Heart Attack Status",
    x = "Angina Status",
    y = "Count of People",
    fill = "Heart Attack Status"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "gray"),  # Set major horizontal gridlines to gray
    panel.grid.minor.y = element_blank(),               # Remove minor horizontal gridlines
    panel.grid.major.x = element_blank(),               # Remove major vertical gridlines
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 16, face = "bold", color = "black")
  )
