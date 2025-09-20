# Load packages ----------------------------------------------------------------



library(shiny)
library(ggplot2)
library(tools)
library(shinythemes)
library(dplyr)
library(DT)


# Load data --------------------------------------------------------------------



Measurements <- read.csv(file = "https://raw.githubusercontent.com/aaron-chen-angus/S3729C-Intake-07/refs/heads/main/TUGbalanceData.csv", header = TRUE, sep = ",")



# Define UI --------------------------------------------------------------------



ui <- fluidPage(
  shinythemes::themeSelector(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(
          "Total Time Taken" = "total_time_taken",
          "Time Seated to Full Extension" = "time_seated_to_full_extension",
          "Time Around Cone" = "time_around_cone",
          "Time Turning to Seated" = "time_turn_to_sit",
          "Time to Sitting Down" = "time_to_sitting_down",
          "Static Balance HUR Score" = "s_open_c90area",
          "Dynamic Balance HUR Score" = "u_closed_c90area",
          "Age" = "age",
          "Gait Velocity" = "gait_velocity"
        ),
        selected = "total_time_taken"
      ),
      
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(
          "Total Time Taken" = "total_time_taken",
          "Time Seated to Full Extension" = "time_seated_to_full_extension",
          "Time Around Cone" = "time_around_cone",
          "Time Turning to Seated" = "time_turn_to_sit",
          "Time to Sitting Down" = "time_to_sitting_down",
          "Static Balance HUR Score" = "s_open_c90area",
          "Dynamic Balance HUR Score" = "u_closed_c90area",
          "Age" = "age",
          "Gait Velocity" = "gait_velocity"
        ),
        selected = "u_closed_c90area"
      ),
      
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c(
          "Risk Level" = "risk_level",
          "Gender" = "gender",
          "First Foot Walk" = "first_foot_walk",
          "Last Foot Walk" = "last_foot_walk",
          "First Foot Around Cone" = "first_foot_around_cone",
          "Last Foot Turn Before Sitting" = "last_foot_turn_before_sitting"
        ),
        selected = "risk_level"
      ),
      
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ),
      
      sliderInput(
        inputId = "size",
        label = "Size:",
        min = 0, max = 5,
        value = 2
      ),
      
      textInput(
        inputId = "plot_title",
        label = "Plot title",
        placeholder = "Enter text to be used as plot title"
      ),
      
      actionButton(
        inputId = "update_plot_title",
        label = "Update plot title"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot", brush = brushOpts(id = "plot_brush")),
      DT::dataTableOutput(outputId = "measurementstable"),
      textOutput(outputId = "avg_x"), # avg of x
      textOutput(outputId = "avg_y"), # avg of y
      verbatimTextOutput(outputId = "lmoutput") # regression output
    )
  )
)



# Define server ----------------------------------------------------------------



server <- function(input, output, session) {
  
  new_plot_title <- eventReactive(
    eventExpr = input$update_plot_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    })
  
  output$scatterplot <- renderPlot({
    ggplot(data = Measurements, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(title = new_plot_title())
  })
  
  output$measurementstable <- renderDataTable({
    brushedPoints(Measurements, brush = input$plot_brush) %>%
      select(name, height, weight, gender, age, total_time_taken, time_seated_to_full_extension, time_around_cone, time_turn_to_sit, time_to_sitting_down, first_foot_walk, last_foot_walk, first_foot_around_cone, last_foot_turn_before_sitting, risk_level, s_open_c90area, u_closed_c90area)
  })
  
  output$avg_x <- renderText({
    avg_x <- Measurements %>% pull(input$x) %>% mean() %>% round(2)
    paste("Average", input$x, "=", avg_x)
  })
  
  output$avg_y <- renderText({
    avg_y <- Measurements %>% pull(input$y) %>% mean() %>% round(2)
    paste("Average", input$y, "=", avg_y)
  })
  
  output$lmoutput <- renderPrint({
    x <- Measurements %>% pull(input$x)
    y <- Measurements %>% pull(input$y)
    print(summary(lm(y ~ x, data = Measurements)), digits = 3, signif.stars = FALSE)
  })
  
}



# Create the Shiny app object --------------------------------------------------



shinyApp(ui = ui, server = server)