# load required packages
library(shiny)
library(tidyverse)

# enable the reactive log to visualize updates
# https://shiny.posit.co/r/articles/improve/debugging/#the-reactive-log
options(shiny.reactlog = TRUE)

# load diamonds dataset
data("diamonds")

# Define UI for application that fits a linear regression to diamond prices by other properties.
# The UI function is identical to that defined in the starting point, all changes are in the
# server function.
ui <-
  fluidPage(# sets up HTML for a 'responsive' app that (should) work on all screen sizes.
    
    # Application title
    titlePanel("Diamond Dashboard"),
    
    # sets up HTML for a basic layout with a side 'menu' bar, and a main area for content
    sidebarLayout(# declare content for the side bar
      sidebarPanel(
        # create select input (dropdown) for picking predictor property
        # take variable names from the actual data file, after excluding
        # columns we don't want to use.
        selectInput(
          "predictor",
          "Predictor",
          choices = diamonds %>% select(-price,-(x:z)) %>% names()
        )
      ),
      
      # declare content for the main panel
      mainPanel(# create a responsive layout that tries to fit content side-by-side if the users'
        # screen is large enough.
        fluidRow(
          # create two columns, of size 8 and 4. Each row is set to have 12 columns, thus
          # this creates a layout where the graph covers 2/3 of the row, and the table the
          # remaining 1/3.
          # If there is not enough space to fit both side by side, the responsive layout
          # declared with fluidRow will place each on their own row, filling the entire
          # row.
          column(8, plotOutput("graph")),
          column(4, verbatimTextOutput("summary"))
        ))))


# Define server logic to draw graph and format summary table
server <- function(input, output) {
  # The `reactive(...)` function creates a reactive "wrapper" around an expression,
  # and parses the expression for the use of other reactive values. In this case, we
  # use the reactive value `input$variable`, so a dependency on that input is auto-
  # matically created. The `model` variable will be updated when `input$reactive`
  # changes, and will in turn trigger updates for reactive values depending on it.
  model <- reactive({
    # stop if no predictor was selected
    req(input$predictor)
    
    .formula <-
      glue::glue("price ~ {input$predictor}") %>% as.formula()
    lm(.formula, diamonds)
  })
  
  output$graph <- renderPlot({
    # stop if no model was created
    req(model(), input$predictor)
    
    # Since we have already fit a model, we can now directly use the fitted
    # model to generate an ab (slope+intercept) line. We therefore replace the
    # `geom_smooth(...)` call with a call to `geom_abline(...)`, using the
    # coefficients on the fitted model.
    coefficients <- model()$coefficients
    ggplot(diamonds, aes(x = .data[[input$predictor]], y = price)) +
      geom_point() +
      geom_abline(intercept = coefficients[1], slope = coefficients[2])
  })
  
  output$summary <- renderPrint({
    # As we already have a model, we merely need to create a summary here.
    model() %>% summary()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
