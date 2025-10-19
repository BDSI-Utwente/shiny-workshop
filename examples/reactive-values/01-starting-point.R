# load required packages
library(shiny)
library(tidyverse)

# enable the reactive log to visualize updates
# https://shiny.posit.co/r/articles/improve/debugging/#the-reactive-log
options(shiny.reactlog = TRUE)

# load diamonds dataset
data("diamonds")

# Define UI for application that fits a linear regression to diamond prices by other properties
ui <-
  fluidPage(# sets up HTML for a 'responsive' app that (should) work on all screen sizes.
    
    # Application title
    titlePanel("Diamond Dashboard"),
    
    # sets up HTML for a basic layout with a side 'menu' bar, and a main area for content
    sidebarLayout(
      # declare content for the side bar
      sidebarPanel(
        # let the user select a predictor variable
        selectInput(
          "predictor",
          "Predictor",
          choices = diamonds %>% select(-price) %>% names(),
          selected = "carat"
        )
      ),
      
      # declare content for the main panel
      mainPanel(plotOutput("graph"),
                verbatimTextOutput("summary"))
      
    ))


# Define server logic to draw graph and format summary table
server <- function(input, output) {
  output$graph <- renderPlot({
    # only continue if a predictor was selected
    req(input$predictor)
    
    # indirection - we need to grab a variable from an 'external' context (.data pronoun)
    ggplot(diamonds, aes(x = .data[[input$predictor]], y = price)) +
      geom_point() +
      # note that geom_smooth() calculates a smoothed fit (using a linear model with
      # `method="lm"`).
      geom_smooth(method = "lm")
  })
  
  # renderPrint() captures the result of the expression (similar to print()), as well as any
  # output generated (e.g. warning messages, etc.). 
  output$summary <- renderPrint({
    req(input$predictor)
    
    # calculate a linear model, and print the summary.
    # indirection - create a formula from a string using user input
    # (glue is a fancier paste)
    .formula <- glue::glue("price ~ {input$predictor}")
    lm(.formula, diamonds) %>% summary()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
