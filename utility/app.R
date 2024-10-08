library(shiny)
library(bslib)

# Define UI for application that calculates the utility of a gamble
ui <- page_sidebar(
  # Application title
  title = "Expected Utility of a Gamble",
  
  # Sidebar with sliders for probabilities and values of events
  sidebar = sidebar(
    numericInput("value_a",
                 "Value of A:",
                 value = 100),
    numericInput("value_b",
                 "Value of B:",
                 value = 0),
    sliderInput("prob_a", 
                "Probability of A Occurring (%):",
                min = 0, max = 100, value = 50, step = 1),
    sliderInput("prob_b",
                "Probability of B Occurring (%):",
                min = 0, max = 100, value = 50, step = 1),
    sliderInput("utility_curvature",
                "Utility Function Curvature:",
                min = 0, max = 2, value = 0.8, step = 0.02)
  ),
  
  # Show the expected value and utility of the gamble
  mainPanel(
    textOutput("expected_value"),
    textOutput("expected_utility"),
    textOutput("sum_check"),
    plotOutput("utility_plot")  # Add plot output to display the utility curve
  )
)

# Define server logic required to calculate the utility of a gamble
server <- function(input, output, session) {
  
  # Reactive values to keep track of the probabilities
  values <- reactiveValues(
    prob_a = 50,
    prob_b = 50
  )
  
  # Observer for prob_a slider
  observeEvent(input$prob_a, {
    isolate({
      total <- values$prob_b + input$prob_a
    })
    
    if (total > 100) {
      values$prob_a <- 100 - values$prob_b
    } else {
      values$prob_a <- input$prob_a
    }
    
    # Update the slider value to maintain total of 100
    updateSliderInput(session, "prob_a", value = values$prob_a)
  })
  
  # Observer for prob_b slider
  observeEvent(input$prob_b, {
    isolate({
      total <- values$prob_a + input$prob_b
    })
    
    if (total > 100) {
      values$prob_b <- 100 - values$prob_a
    } else {
      values$prob_b <- input$prob_b
    }
    
    # Update the slider value to maintain total of 100
    updateSliderInput(session, "prob_b", value = values$prob_b)
  })
  
  # Calculate the expected value of the gamble
  output$expected_value <- renderText({
    # Normalize probabilities to proportions (0-1)
    prob_a_normalized <- values$prob_a / 100
    prob_b_normalized <- values$prob_b / 100
    
    # Calculate the expected value
    expected_value <- (prob_a_normalized * input$value_a) + 
      (prob_b_normalized * input$value_b)
    
    paste("Expected Value of the Gamble: $", round(expected_value, 2))
  })
  
  # Calculate the utility of the gamble
  output$expected_utility <- renderText({
    # Normalize probabilities to proportions (0-1)
    prob_a_normalized <- values$prob_a / 100
    prob_b_normalized <- values$prob_b / 100
    
    # Calculate the utility for each value
    utility_a <- input$value_a ^ input$utility_curvature
    utility_b <- input$value_b ^ input$utility_curvature
    
    # Calculate the expected utility
    expected_utility <- (prob_a_normalized * utility_a) + 
      (prob_b_normalized * utility_b)
    
    paste("Expected Utility of the Gamble:", round(expected_utility, 2))
  })
  
  # Check the sum of probabilities
  output$sum_check <- renderText({
    paste("Total Probability:", values$prob_a + values$prob_b, "%")
  })
  
  # Generate a plot illustrating the utility function
  output$utility_plot <- renderPlot({
    # Define a sequence of values for X (representing value)
    x_values <- seq(0, max(input$value_a, input$value_b) * 1.5, by = 1)
    
    # Calculate the corresponding Y values for the utility function
    y_values <- x_values ^ input$utility_curvature
    
    # Calculate the current expected value and utility based on input
    prob_a_normalized <- values$prob_a / 100
    prob_b_normalized <- values$prob_b / 100
    expected_value <- (prob_a_normalized * input$value_a) + 
      (prob_b_normalized * input$value_b)
    
    # Calculate the utility at the expected value
    expected_utility <- expected_value ^ input$utility_curvature
    
    # Create the plot
    plot(x_values, y_values, type = "l", col = "navy", lwd = 2,
         xlab = "Expected Value", 
         ylab = "Expected Utility",
         main = "Utility as a Function of Expected Value")
    
    # Add a point for the expected value and utility
    points(expected_value, 
           expected_utility, 
           col = "maroon", 
           pch = 19, cex = 1.5)
    
    # Draw horizontal and vertical line segments toward the point
    segments(x0 = 0, 
             y0 = expected_utility, 
             x1 = expected_value, 
             y1 = expected_utility, 
             col = "black", lty = 2)
    segments(x0 = expected_value, 
             y0 = 0, 
             x1 = expected_value, 
             y1 = expected_utility, 
             col = "black", lty = 2)
     
    # # Annotate the intersection with the axis
    # text(max(input$value_a, input$value_b) * .2, # X position
    #      expected_utility + max(input$value_a, input$value_b) * .02, # Y position
    #      labels = round(expected_utility, 2), 
    #      pos = 2, 
    #      col = "black")
    
    
    # Annotate the point
    text(
      expected_value * 1.02,
      expected_utility * .98,
      labels = paste0(
        "(",
        round(expected_value, 2),
        ", ",
        round(expected_utility, 2),
        ")"
      ),
      pos = 4,
      col = "black"
    )
    
    if (input$utility_curvature == 1) {
      text(
        max(x_values) * .6,
        max(y_values) * .25,
        labels = "Risk Neutral",
        pos = 4,
        col = "black"
      )
    } else if (input$utility_curvature >= 1) {
      text(
        max(x_values) * .6,
        max(y_values) * .25,
        labels = "Risk Seeking",
        pos = 4,
        col = "black"
      )
    } else {
      text(
        max(x_values) * .6,
        max(y_values) * .25,
        labels = "Risk Averse",
        pos = 4,
        col = "black"
      )
    }
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# runGitHub("simonvbaal/shiny_utility", subdir = "utility")
