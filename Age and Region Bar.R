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

df <- df %>%
  mutate(
    age_group = case_when(
      age <= 12 ~ "Kids(1-12)",
      age > 12 & age <= 19 ~ "Teens(13-19)",
      age > 19 & age <= 33 ~ "Young Adults(20-33)",
      age > 33 & age <= 48 ~ "Adults(34-48)",
      age > 48 & age <= 60 ~ "Elderly(49-60)"
    )
  )

summary_df <- df %>%
  group_by(age_group, region) %>%
  summarise(
    heart_attack_count = sum(heart_attack),
    .groups = "drop"
  )

all_age_groups <- c("Kids(1-12)", "Teens(13-19)", "Young Adults(20-33)", "Adults(34-48)", "Elderly(49-60)")
all_regions <- unique(df$region)

full_summary_data <- expand.grid(age_group = all_age_groups, region = all_regions) %>%
  left_join(summary_df, by = c("age_group", "region")) %>%
  replace_na(list(heart_attack_count = 0))  

region_order <- full_summary_data %>%
  group_by(region) %>%
  summarise(total_heart_attacks = sum(heart_attack_count)) %>%
  arrange(desc(total_heart_attacks)) %>%
  pull(region)

full_summary_data$region <- factor(full_summary_data$region, levels = region_order)

full_summary_data$age_group <- factor(full_summary_data$age_group, 
                                      levels = c("Kids(1-12)", "Teens(13-19)", "Young Adults(20-33)", "Adults(34-48)", "Elderly(49-60)"))

ggplot(full_summary_data, aes(x = age_group, y = heart_attack_count, fill = region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2", name = "Region") +
  geom_text(aes(label = heart_attack_count), position = position_stack(vjust = 0.5)) +  # Add annotations
  labs(
    title = "Heart Attack Distribution by Age Group and Region",
    x = "Age Group",
    y = "Heart Attack Count",
    caption = "Source: Your Dataset"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

