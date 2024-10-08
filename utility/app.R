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
                min = 0, max = 2, value = 0.6, step = 0.02)
  ),
  
  # Show the expected value and utility of the gamble
  layout_columns(
    card(textOutput("expected_value"),
         textOutput("expected_utility_sure"),
         textOutput("expected_utility"),
         textOutput("sum_check")),
    card(
      card_header("Utility as a Function of Value"),
      plotOutput("utility_plot")), # Add plot output to display the utility curve
    col_widths = c(4, 8),
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
    updateSliderInput(session, "prob_a", 
                      value = values$prob_a)
  })
  
  # Observer for prob_b slider
  observeEvent(input$prob_b, {
    isolate({
      total <- 
        values$prob_a + input$prob_b
    })
    
    if (total > 100) {
      values$prob_b <- 100 - values$prob_a
    } else {
      values$prob_b <- input$prob_b
    }
    
    # Update the slider value to maintain total of 100
    updateSliderInput(session, 
                      "prob_b", 
                      value = values$prob_b)
  })
  
  # Calculate the expected value of the gamble
  output$expected_value <- renderText({
    
    # Normalize probabilities to proportions (0-1)
    prob_a_normalized <- 
      values$prob_a / 100
    prob_b_normalized <- 
      values$prob_b / 100
    
    # Calculate the expected value
    expected_value <- 
      (prob_a_normalized * input$value_a) + 
      (prob_b_normalized * input$value_b)
    
    paste("Expected Value of the Gamble: $", 
          round(expected_value, 2))
  })
  
  output$expected_utility_sure <- renderText({
    # Calculate the utility of the certain equivalent of expected value
    expected_value <- 
      (values$prob_a / 100) * input$value_a + 
      (values$prob_b / 100) * input$value_b
    utility <-
      sign(expected_value) * abs(expected_value) ^ input$utility_curvature
    paste0("Utility of Equivalent Sure Amount ($",
          round(expected_value, 2), "): ", 
          round(utility, 2))
  })
  
  # Calculate the utility of the gamble
  output$expected_utility <- renderText({
    # Normalize probabilities to proportions (0-1)
    prob_a_normalized <- values$prob_a / 100
    prob_b_normalized <- values$prob_b / 100
    
    # Calculate the expected utility
    expected_utility_a <- 
      (sign(input$value_a) * abs(input$value_a) ^ input$utility_curvature) *
      prob_a_normalized
    expected_utility_b <-
      (sign(input$value_b) * abs(input$value_b) ^ input$utility_curvature) *
      prob_b_normalized
    expected_utility <- 
      expected_utility_a + expected_utility_b
    
    paste("Expected Utility of the Gamble:", 
          round(expected_utility, 2))
  })

  
  # Check the sum of probabilities
  output$sum_check <- renderText({
    paste("Total Probability:", 
          values$prob_a + values$prob_b, "%")
  })
  
  # Generate a plot illustrating the utility function
  output$utility_plot <- renderPlot({
    
    # Calculate the current expected value and utility based on input
    prob_a_normalized <- values$prob_a / 100
    prob_b_normalized <- values$prob_b / 100
    expected_value_a <- (prob_a_normalized * input$value_a)
    expected_value_b <- (prob_b_normalized * input$value_b)
    expected_value <- expected_value_a + expected_value_b
    
    # Calculate the utility of each event
    utility_a <- 
      sign(input$value_a) * abs(input$value_a) ^ input$utility_curvature
    utility_b <-
      sign(input$value_b) * abs(input$value_b) ^ input$utility_curvature
    expected_utility <- 
      utility_a * prob_a_normalized + 
      utility_b * prob_b_normalized
    
    if (min(input$value_a, input$value_b) >= 0) {
      x_min <- 0
    } else {
      x_min <- min(input$value_a, input$value_b) * 1.5
    }
    # Define a sequence of values for X (representing value) with a broader negative range
    x_values <- seq(x_min, 
                    max(input$value_a, input$value_b) * 1.5, by = 1)
    
    # Calculate the corresponding Y values for the utility function
    y_values <- sign(x_values) * abs(x_values) ^ input$utility_curvature
    
    # Create the plot
    plot(x_values, y_values, type = "l", col = "navy", lwd = 2,
         xlab = "Value", ylab = "Utility",
         xlim = c(min(x_values), max(x_values)), 
         ylim = c(min(y_values), max(y_values)))
    
    # Add a point for the expected value and utility
    points(min(x_values), 
           expected_utility, 
           col = "maroon", pch = 3, cex = 1.5)
    points(expected_value, 
           sign(expected_value) * abs(expected_value) ^ input$utility_curvature, 
           col = "grey", pch = 19, cex = 1.5)
    
    # Draw horizontal and vertical line segments toward the point
    
    # Utility of A
    segments(x0 = min(x_values), y0 = utility_a, 
             x1 = input$value_a, y1 = utility_a, 
             col = "black", lty = 2)
    segments(x0 = input$value_a, y0 = min(y_values), 
             x1 = input$value_a, y1 = utility_a, 
             col = "black", lty = 2)
    # Annotate Utility of A
    text(min(x_values) + .01 * (max(x_values) - min(x_values)), 
         utility_a + .015 * (max(y_values) - min(y_values)), 
         labels = paste0("U(A) = ", round(utility_a, 2)), 
         pos = 4, col = "black")
    
    # Utility of B
    segments(x0 = min(x_values), y0 = utility_b, 
             x1 = input$value_b, y1 = utility_b, 
             col = "black", lty = 2)
    segments(x0 = input$value_b, y0 = min(y_values),
             x1 = input$value_b, y1 = utility_b, 
             col = "black", lty = 2)
    # Annotate Utility of B
    text(min(x_values) + .01 * (max(x_values) - min(x_values)), 
         utility_b + .015 * (max(y_values) - min(y_values)), 
         labels = paste0("U(B) = ", round(utility_b, 2)), 
         pos = 4, col = "black")
    
    # Annotate Expected Utility
    text(min(x_values) + .01 * (max(x_values) - min(x_values)), 
         expected_utility * 0.98, 
         labels = paste0("EU = ", 
                         round(expected_utility, 2)), 
         pos = 4, col = "black")
    
    # Annotate EU of Expected Value
    text(expected_value + .01 * (max(x_values) - min(x_values)), 
         sign(expected_value) * abs(expected_value) ^ input$utility_curvature, 
         labels = paste0("EU(EV) = ", 
                         round(sign(expected_value) * abs(expected_value) ^ input$utility_curvature, 2)), 
         pos = 4, col = "black")
    
    # Add risk type annotation based on utility curvature
    risk_type <- if (input$utility_curvature == 1)
      "Risk Neutral"
    else if (input$utility_curvature > 1)
      "Risk Seeking"
    else
      "Risk Averse"
    text(
      (max(x_values) - min(x_values)) * 0.7,
      (max(y_values) - min(y_values)) * 0.12,
      labels = risk_type,
      pos = 4,
      col = "black"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# runGitHub("simonvbaal/shiny_utility", subdir = "utility")
