install.packages("tidyverse")
install.packages("janitor")  
library(tidyverse)
install.packages("systemfonts")
library(systemfonts)
library(janitor) 

CBD <- read.csv("data/heart_attack_nigeria_youth_vs_adult.csv")
colnames(CBD)

CBD <- clean_names(CBD)

# Convert relevant columns to numeric and handle missing values
CBD <- CBD %>%
  mutate(
    diet = as.numeric(diet),
    sleep_hours = as.numeric(sleep_hours),
    physical_activity = as.numeric(physical_activity),
    daily_water_intake = as.numeric(daily_water_intake),
    bmi = as.numeric(bmi),
    alcohol_consumption = as.numeric(alcohol_consumption),
    smoking = as.numeric(smoking),
    stress_level = as.numeric(stress_level)
  ) %>%
  mutate(
    diet = ifelse(is.na(diet), 0, diet),
    sleep_hours = ifelse(is.na(sleep_hours), mean(sleep_hours, na.rm = TRUE), sleep_hours),
    physical_activity = ifelse(is.na(physical_activity), 0, physical_activity),
    daily_water_intake = ifelse(is.na(daily_water_intake), 0, daily_water_intake),
    bmi = ifelse(is.na(bmi), 0, bmi),
    alcohol_consumption = ifelse(is.na(alcohol_consumption), 0, alcohol_consumption),
    smoking = ifelse(is.na(smoking), 0, smoking),
    stress_level = ifelse(is.na(stress_level), mean(stress_level, na.rm = TRUE), stress_level)
  )

# Define weights for the health index
weights <- c(
  diet = 0.2,
  sleep_hours = 0.1,
  physical_activity = 0.2,
  daily_water_intake = 0.1,
  bmi = 0.15,
  alcohol_consumption = 0.1,
  smoking = 0.1,
  stress_level = 0.05
)

# Create the health index
CBD <- CBD %>%
  mutate(
    health_index = diet * weights["diet"] +
      sleep_hours * weights["sleep_hours"] +
      physical_activity * weights["physical_activity"] +
      daily_water_intake * weights["daily_water_intake"] +
      bmi * weights["bmi"] +
      alcohol_consumption * weights["alcohol_consumption"] +
      smoking * weights["smoking"] +
      stress_level * weights["stress_level"]
  )

# Filter rows with finite health index
CBD <- CBD %>%
  filter(is.finite(health_index))

# Visualization: Health Index Distribution by Heart Attack
CBD %>%
  ggplot(aes(x = health_index, fill = as.factor(heart_attack))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Health Index vs. Heart Attack",
    x = "Health Index",
    fill = "Heart Attack"
  ) +
  theme_minimal()

# New: Create bins for the health index (you can adjust the number of bins)
CBD <- CBD %>%
  mutate(health_index_bin = cut(
    health_index, 
    breaks = seq(min(health_index, na.rm = TRUE), max(health_index, na.rm = TRUE), length.out = 10),
    include.lowest = TRUE
  ))

# New: Calculate percentages within each bin
health_index_summary <- CBD %>%
  group_by(health_index_bin, heart_attack) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(health_index_bin) %>%
  mutate(percentage = count / sum(count) * 100)



# Create bins from 0 to 0.49, 0.5 to 1.49, etc., and calculate the midpoint for labels
CBD <- CBD %>%
  mutate(
    health_index_bin = cut(
      health_index,
      breaks = seq(floor(min(health_index, na.rm = TRUE)), ceiling(max(health_index, na.rm = TRUE)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    ),
    health_index_midpoint = as.numeric(sub(".*\\[(.*),.*\\)", "\\1", health_index_bin)) + 0.5
  )

# Calculate the percent chance of having a heart attack for each bin
health_index_summary <- CBD %>%
  group_by(health_index_midpoint) %>%
  summarise(
    total = n(),
    heart_attack_count = sum(heart_attack == "True"),
    percent_heart_attack = (heart_attack_count / total) * 100,
    .groups = "drop"
  )

# Plot the percent chance of having a heart attack
X <- health_index_summary %>%
  ggplot(aes(x = health_index_midpoint, y = percent_heart_attack)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Percent Chance of Heart Attack by Health Index",
    x = "Health Index Midpoint",
    y = "Percent Chance of Heart Attack (%)"
  ) +
  scale_x_continuous(breaks = health_index_summary$health_index_midpoint) +
  theme_minimal()

print(X)

















