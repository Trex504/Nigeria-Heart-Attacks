install.packages("tidyverse")
install.packages("janitor")  
library(tidyverse)
install.packages("systemfonts")
library(systemfonts)
library(janitor) 

CBD <- read.csv("data/heart_attack_nigeria_youth_vs_adult.csv")
colnames(CBD)



#Bar Chart




# Load ggplot2
library(ggplot2)

# Group Age into Youth and Adult (Assume Age < 30 is Youth)
CBD$Age_Group <- ifelse(CBD$Age < 30, "Youth", "Adult")

# Create the bar chart
ggplot(CBD, aes(x = Age_Group, fill = as.factor(Heart_Attack))) +
  geom_bar(position = "dodge") +  # Separate bars for each Heart_Attack status
  labs(
    title = "Heart Attack Prevalence by Age Group",
    subtitle = "Comparing Youth and Adults in Nigeria",
    x = "Age Group",
    y = "Count",
    fill = "Heart Attack"
  ) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("No", "Yes")) +  # Custom colors
  theme_minimal()  # Apply a clean theme




# Box and Whisker




# Load ggplot2
library(ggplot2)

# Create the box plot
ggplot(CBD, aes(x = Gender, y = Blood_Pressure, fill = Gender)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +  # Box plot with highlighted outliers
  labs(
    title = "Distribution of Blood Pressure by Gender",
    subtitle = "Analyzing potential differences in blood pressure between genders",
    x = "Gender",
    y = "Blood Pressure (mmHg)",
    fill = "Gender"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for better aesthetics
  theme_minimal() +  # Clean theme
  theme(legend.position = "none")  # Remove legend for simplicity






#BMI V AGE





# Sample 10,000 rows from the dataset (or adjust the number)
set.seed(123)  # For reproducibility
CBD_sample <- CBD[sample(nrow(CBD), 30000), ]

# Plot with the sampled data
ggplot(CBD_sample, aes(x = Age, y = BMI, color = Gender)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black") +
  labs(
    title = "BMI vs Age by Gender (Sampled Data)",
    subtitle = "Analyzing trends using a random subset",
    x = "Age (years)",
    y = "BMI",
    color = "Gender"
  ) +
  theme_minimal()







