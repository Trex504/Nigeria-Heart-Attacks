library(dplyr)
library(ggplot2)

average_scores <- df %>%
  group_by(angina) %>%
  summarise(avg_score = mean(health_awareness_score, na.rm = TRUE))

ggplot(average_scores, aes(
  x = factor(angina, labels = c("Without Angina", "With Angina")),
  y = avg_score - 2.9,
  fill = factor(angina)
)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(avg_score, 1)), vjust = -0.5, size = 4, color = "black") +
  scale_fill_manual(values = c("skyblue", "#D32F2F"), guide = "none") +
  labs(
    title = "Comparison of Health Awareness Scores by Angina Status",
    x = "Angina Status",
    y = "Average Health Awareness Score"
  ) +
  theme_dark() +
  theme(
    panel.grid.major.y = element_line(color = "lightgray"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 16, face = "bold", color = "black")
  )
