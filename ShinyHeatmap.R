library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(janitor)
library(tidyverse)
library(sf)
library(gganimate)
library(DescTools)
library(shiny)

#install.packages("shiny")

df <- read_csv("data/heart_attack.csv")
df <- clean_names(df)

ui <- fluidPage(
  titlePanel("Comparing Personal Lives With Health Awareness"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_col", "Select X-axis Column:", choices = colnames(df), selected = "occupation"),
      selectInput("y_col", "Select Y-axis Column:", choices = colnames(df), selected = "income_level"),
      selectInput("fill_col", "Select Fill Column:", choices = colnames(df), selected = "health_awareness")
    ),
    mainPanel(
      plotOutput("heatmap", height = "600px", width = "100%")  # Make the plot larger
    )
  )
)

# Server logic for the app
server <- function(input, output) {
  output$heatmap <- renderPlot({
    # Aggregate the data dynamically based on user inputs
    heatmap_data <- df %>%
      group_by(across(all_of(c(input$x_col, input$y_col)))) %>%
      summarize(avg_fill = mean(.data[[input$fill_col]], na.rm = TRUE), .groups = "drop")
    
    # Create the heatmap
    ggplot(heatmap_data, aes_string(x = input$x_col, y = input$y_col, fill = "avg_fill")) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "red", high = "blue") +
      labs(
        title = paste("Heatmap of", input$fill_col, "by", input$x_col, "and", input$y_col),
        x = input$x_col,
        y = input$y_col,
        fill = paste("Avg.", input$fill_col)
      ) +
      theme_dark(base_size = 16) +  
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16),  
        plot.title = element_text(size = 18)  
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

