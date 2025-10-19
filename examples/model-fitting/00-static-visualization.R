library(shiny)
library(tidyverse)

data("diamonds")

predictors <- diamonds %>%
  select(-price) %>%
  names()

ui <- fluidPage(
  titlePanel("Diamond price prediction"),
  mainPanel(
    plotOutput("plot", height = "800px"),
    verbatimTextOutput("summary")
  )
)

server <- function(input, output) {
  model <- reactive({
    lm(price ~ carat + cut + color, data = diamonds)
  })
  
  output$plot <- renderPlot({
    req(model())
    
    # plot default plot for models, usually some form of fit diagnostic, but
    # it depends on the predictors selected
    par(mfcol = c(2, 2)) # plot.lm creates 4 panels for debugging, put them in a 2x2 grid
    model() %>% plot()
  })
  
  output$summary <- renderPrint({
    req(model())
    
    model() %>% summary()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
