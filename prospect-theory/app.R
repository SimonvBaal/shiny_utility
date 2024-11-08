# This is a Shiny web application that helps you visualise prospect theory

library(shiny)
library(bslib)
library(bsicons)
library(gitlink)


# One can define functions here (as I should have done)

# Define UI for application that calculates the value of a prospect
ui <- page_sidebar(
  ribbon_css("https://github.com/SimonvBaal/shiny_utility"),
  
  # Application theme
  theme = bs_theme(
    bootswatch = "darkly",
    #primary = "#00a6a6",
    #secondary = "#f0f0f0",
    base_font = "Helvetica Neue",
    base_font_size = 9,
    heading_font = "Helvetica Neue",
    heading_font_size = 14,
  ),
  
  # Application title
  title = "Prospect Theory",
  
  # Sidebar with sliders for probabilities and values of events
  sidebar = sidebar(
    numericInput("value_a", "Outcome A:", value = 100),
    numericInput("value_b", "Outcome B:", value = -100),
    sliderInput(
      "prob_a",
      "Probability of A Occurring (%):",
      min = 0,
      max = 100,
      value = 50,
      step = 1
    ),
    sliderInput(
      "prob_b",
      "Probability of B Occurring (%):",
      min = 0,
      max = 100,
      value = 50,
      step = 1
    ),
    sliderInput(
      "utility_curvature",
      "Diminishing Sensitivity:",
      min = 0,
      max = 2,
      value = 0.6,
      step = 0.02
    ),
    sliderInput(
      "loss_aversion",
      "Loss Aversion:",
      min = 0,
      max = 3,
      value = 2,
      step = 0.02
    ),
  ),
  
  # Show the expected value and utility of the gamble
  layout_columns(
    card(
      card_header("Value of a Gamble, with Gains and Losses", class = "h6"),
      plotOutput("value_plot")
    ),
    # Add plot output to display the utility curve
    value_box(
      title = "Note:",
      tags$div(textOutput("loss_note"), style = "font-size: 16px; font-weight: bold; color: white;")
    ),
    value_box(
      title = "Expected Value",
      value =
        tags$div(textOutput("expected_value"), style = "font-size: 28px; font-weight: bold; color: white;"),
      showcase = bs_icon("piggy-bank-fill")
    ),
    value_box(
      title = "Prospect Value",
      value =
        tags$div(textOutput("prospect_value"), style = "font-size: 24px; font-weight: bold; color: white;"),
      showcase = bs_icon("emoji-smile"),
      theme = "primary"
    ),
    value_box(
      title = "Reference Point",
      tags$div(textOutput("reference_note"), style = "font-size: 16px; font-weight: bold; color: white;")
    ),
    col_widths = c(10, 2, 4, 4, 4),
    row_heights = c(3, .9)
  )
)

# Define server logic required to calculate the utility of a gamble
server <- function(input, output, session) {
  #bs_themer()

  observe(updateSliderInput(session, 
                              inputId = "prob_a", 
                            value = 100 - input$prob_b))
    
  output$prob_b <- renderUI({
      sliderInput(inputId = "prob_b", 
                  label = "Probability of B Occurring", 
                  min = 0, max = 100, 
                  value = 100 - input$prob_a)
  })
  
  observe(updateSliderInput(session, 
                            inputId = "prob_b", 
                            value = 100 - input$prob_a))
  
 
  # Calculate the expected value of the gamble
  output$expected_value <- renderText({

    # Normalize probabilities to proportions (0-1)
    prob_a_normalized <- 
      input$prob_a / 100
    prob_b_normalized <- 
      input$prob_b / 100
    
        
    # Calculate the expected value
    expected_value <- 
      (prob_a_normalized * input$value_a) + 
      (prob_b_normalized * input$value_b)
    
    paste0("$", round(expected_value, 2))
  })
  
  output$expected_utility_sure <- renderText({
    # Calculate the utility of the certain equivalent of expected value
    expected_value <- 
      (input$prob_a / 100) * input$value_a + 
      (input$prob_b / 100) * input$value_b
    utility <-
      sign(expected_value) * abs(expected_value) ^ input$utility_curvature
    paste0("U($",
          round(expected_value, 2), "): ", 
          round(utility, 2))
  })
  
  output$prospect_value <- renderText({
    
    # Normalize probabilities to proportions (0-1)
    prob_a_normalized <- 
      input$prob_a / 100
    prob_b_normalized <- 
      input$prob_b / 100
    
    # Calculate the prospect value
    if (sign(input$value_a) == -1) {
      value_a <- -1 * input$loss_aversion * abs(input$value_a) ^ input$utility_curvature
    } else {
      value_a <- input$value_a ^ input$utility_curvature
    }
    
    if (sign(input$value_b) == -1) {
      value_b <- -1 * input$loss_aversion * abs(input$value_b) ^ input$utility_curvature
    } else {
      value_b <- input$value_b ^ input$utility_curvature
    }
    
    prospect_value <- 
      value_a * prob_a_normalized + 
      value_b * prob_b_normalized
       
    paste0(prob_a_normalized, "*v(", input$value_a, ") + ", 
              prob_b_normalized, "*v(", input$value_b, ") = ", round(prospect_value, 2))
  })

  
  #======================================== Prospect Theory Notes
  
  output$loss_note <- renderText({
    risk_preference <- "Diminishing sensitivity influences value similar to EU theory - in gains v(EV) > v(gamble) for risk averse individuals. In losses, people are risk seeking! "
    loss_aversion_type <- if (input$loss_aversion == 1)
      "If the loss aversion parameter is 1, the individual is neutral to gains vs losses. A loss of $100 is equally as bad as a gain of $100 is good."
    else if (input$loss_aversion > 1)
      "If the loss aversion parameter is greater than 1, the individual is loss averse. A loss of $100 is worse than a gain of $100 is good."
    else
      "If the loss aversion parameter is smaller than 1, the individual is gain seeking. A loss of $100 is less bad than a gain of $100 is good."
    paste0(risk_preference, loss_aversion_type)
  })
  
  output$reference_note <- renderText({
    reference_note <- "The reference point (0) is from where we evaluate gains and losses. This is different from the Expected Utility of a gamble, which is added to one's current wealth."
    reference_note
  })
  
  #======================================= Plot
  
  # Generate a plot illustrating the utility function
  output$value_plot <- renderPlot({
    
    # Calculate the current expected value and utility based on input
    prob_a_normalized <- input$prob_a / 100
    prob_b_normalized <- input$prob_b / 100
    expected_value_a <- (prob_a_normalized * input$value_a)
    expected_value_b <- (prob_b_normalized * input$value_b)
    expected_value <- expected_value_a + expected_value_b
    
    # Calculate the prospect value
    if (sign(input$value_a) == -1) {
      value_a <- -1 * input$loss_aversion * abs(input$value_a) ^ input$utility_curvature
    } else {
      value_a <- input$value_a ^ input$utility_curvature
    }
    
    if (sign(input$value_b) == -1) {
      value_b <- -1 * input$loss_aversion * abs(input$value_b) ^ input$utility_curvature
    } else {
      value_b <- input$value_b ^ input$utility_curvature
    }
    
    prospect_value <- 
      value_a * prob_a_normalized + 
      value_b * prob_b_normalized
    
    if (min(input$value_a, input$value_b) >= 0) {
      x_min <- 0
    } else {
      x_min <- min(input$value_a, input$value_b) * 1.5
    }
    # Define a sequence of values for X (representing value) with a broader negative range
    x_values <- seq(x_min, 
                    ifelse(sign(max(input$value_a, input$value_b)) == 1, 
                           max(input$value_a, input$value_b) * 1.5,
                           max(input$value_a, input$value_b) * .75), by = 1)
    
    # Calculate the corresponding Y values for the value function
    y_values <- (abs(x_values) ^ input$utility_curvature) * 
               ifelse(x_values < 0, -input$loss_aversion, 1)
    
    # Create the plot
    par(family = "Helvetica")
    plot(x_values, y_values, type = "l", col = "navy", lwd = 2,
         xlab = "Loss / Gain", ylab = "Value",
         xlim = c(min(x_values), max(x_values)), 
         ylim = c(min(y_values), max(y_values)))
    
    # Add a point for prospect value
    points(min(x_values), 
           prospect_value, 
           col = "maroon", pch = 3, cex = 1.5)
    points(expected_value, 
           (abs(expected_value) ^ input$utility_curvature) * 
                   ifelse(expected_value < 0, -input$loss_aversion, 1), 
            col = "grey", pch = 19, cex = 1.5)
     
    # Draw horizontal and vertical line segments toward the point
    
    # Value of A
    segments(x0 = min(x_values), y0 = value_a, 
             x1 = input$value_a, y1 = value_a, 
             col = "black", lty = 2)
    segments(x0 = input$value_a, y0 = min(y_values), 
             x1 = input$value_a, y1 = value_a, 
             col = "black", lty = 2)
    
    # Annotate Value of A
    text(min(x_values) + .01 * (max(x_values) - min(x_values)), 
         value_a + .03 * (max(y_values) - min(y_values)), 
         labels = paste0("v(A) = ", round(value_a, 2)), 
         pos = 4, col = "black")
    
    # Value of B
    segments(x0 = min(x_values), y0 = value_b, 
             x1 = input$value_b, y1 = value_b, 
             col = "black", lty = 2)
    segments(x0 = input$value_b, y0 = min(y_values),
             x1 = input$value_b, y1 = value_b, 
             col = "black", lty = 2)
    
    # Annotate Utility of B
    text(min(x_values) + .01 * (max(x_values) - min(x_values)), 
         value_b + .03 * (max(y_values) - min(y_values)), 
         labels = paste0("v(B) = ", round(value_b, 2)), 
         pos = 4, col = "black")
    
    # Add axis lines
    abline(h = 0, v = 0, col = "black", lty = 1)
    
    
    # Annotate Prospect Value
    text(min(x_values) + .01 * (max(x_values) - min(x_values)), 
         prospect_value * 0.98, 
         labels = paste0("v(gamble) = ", 
                         round(prospect_value, 2)), 
         pos = 4, col = "black")
    
    # Annotate Value of EV
    text(expected_value + .01 * (max(x_values) - min(x_values)), 
         (abs(expected_value) ^ input$utility_curvature) * 
           ifelse(expected_value < 0, -input$loss_aversion, 1), 
         labels = paste0("v(EV) = ", 
                         round((abs(expected_value) ^ input$utility_curvature) * 
                                 ifelse(expected_value < 0, -input$loss_aversion, 1), 2)), 
         pos = 4, col = "black")
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# runGitHub("simonvbaal/shiny_utility", subdir = "prospect-theory")
