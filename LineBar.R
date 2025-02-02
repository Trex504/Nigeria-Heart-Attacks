library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(janitor)
library(tidyverse)
library(sf)
library(gganimate)

df <- read_csv("data/heart_attack.csv")
df <- clean_names(df)

df_avg_chol <- df %>%
  group_by(age) %>%
  summarize(avg_cholesterol = mean(cholesterol, na.rm = TRUE))

# Calculate the number of heart attack cases per age
df_heart_attack_count <- df %>%
  group_by(age) %>%
  summarize(heart_attack_cases = sum(heart_attack == TRUE, na.rm = TRUE))

# Merge the two data frames
df_combined <- merge(df_avg_chol, df_heart_attack_count, by = "age")

# Shift cholesterol values down by subtracting 200
df_combined$shifted_cholesterol <- df_combined$avg_cholesterol - 175

# Shift heart attack cases down by subtracting 75
df_combined$shifted_heart_attack_cases <- df_combined$heart_attack_cases - 75

# Create the line and bar graph combined
ggplot(df_combined) +
  # Bar graph for shifted heart attack cases
  geom_bar(aes(x = age, y = shifted_heart_attack_cases), stat = "identity", fill = "skyblue", alpha = 0.7, width = 0.5) +
  # Line graph for shifted average cholesterol
  geom_line(aes(x = age, y = shifted_cholesterol), color = "red", size = 1.5) +  # Line with shifted cholesterol
  # Add points for the line
  geom_point(aes(x = age, y = shifted_cholesterol), color = "red", size = 3) +
  # Customizing axis labels
  scale_y_continuous(
    name = "Heart Attack Cases", 
    sec.axis = sec_axis(~ ., name = "Average Cholesterol")  # Secondary axis for cholesterol
  ) +
  labs(title = "Heart Attack Cases and Average Cholesterol by Age", x = "Age") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "skyblue"),  # Color for heart attack y-axis
    axis.title.y.right = element_text(color = "red"),  # Color for cholesterol y-axis
    legend.position = "none"
  )

