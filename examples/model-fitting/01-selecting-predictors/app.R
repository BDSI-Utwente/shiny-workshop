library(shiny)
library(tidyverse)
library(here)

SMARTc <- vroom::vroom(here::here("SMARTc.csv")) |> 
  mutate(EVENT = as.logical(EVENT))
data("diamonds")

# get names of variables in the diamonds dataset for later use
# we exclude price, because it is the outcome measure here
predictors <- SMARTc %>%
  select(-EVENT, -VEVENT) %>%
  names()


ui <- fluidPage(titlePanel("Event prediction"),
                
                # We'll follow a pretty much standard shiny layout. This includes:
                # - a sidebar on the left with configurable inputs
                # - a main content area on the center/right
                sidebarLayout(
                  sidebarPanel(
                    
                    # note that we could also have used a dropdown;
                    selectInput(
                      "predictors",
                      "Select predictors...",
                      predictors,
                      multiple = TRUE
                    )
                  ),
                  
                  # on the content side, we'll model diagnostics; the standard four
                  # linear model plots, and the model summary.
                  mainPanel(
                    plotOutput("plot", height = "800px"),
                    verbatimTextOutput("summary")
                  )
                ))

server <- function(input, output) {
  model <- reactive({
    # there is no model if there are no predictors selected
    req(input$predictors)
    
    # we need some way to programmatically alter the linear model. We have two
    # main options here, we could 'pull out' the data column for the predictor
    # and outcome, and run a linear model on that:
    #
    # .outcome <- diamonds$price
    # .predictors <- diamonds[[input$predictor]]
    #
    # lm(.outcome ~ .predictors)
    #
    # This would work, but the output we get now uses '.outcome' and '.predictors'
    # instead of the actual variable labels. We could further process the output
    # to remove this, but we'll leave that for now.
    #
    # The second option is to dynamically change the formula we use;
    # instead of `.outcome ~ .predictor`, we can use `price ~ <variable>`, where
    # <variable> would be replaced with whatever predictor(s) the user selected.
    #
    # To do this, we paste together the formula as a string first, and then create
    # a formula with the `as.formula()` function. Take the selected predictors
    # (e.g., `c("carat", "cut")`), and collapse them into a single string
    # (e.g., "carat + cut").
    predictors_formula <-
      stringr::str_flatten(input$predictors, " + ")
    
    # combine outcome and predictor(s), then create the formula
    formula <-
      glue::glue("EVENT ~ {predictors_formula}") %>% as.formula()
    
    # note that this is analogous to `lm(price ~ carat + cut, ...)`, but we have
    # to jump through some extra hoops to deal with variable inputs.
    glm(formula, data = SMARTc, family = "binomial")
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
