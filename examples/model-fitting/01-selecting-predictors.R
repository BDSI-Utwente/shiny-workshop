library(shiny)
library(tidyverse)

data("diamonds")

predictors <- diamonds %>%
  select(-price) %>%
  names()

ui <- fluidPage(titlePanel("Diamond price prediction"),
                sidebarLayout(
                  sidebarPanel(
                    # Create a group of checkboxes
                    checkboxGroupInput("predictors", "Select predictors...", predictors),
                    
                    # We can use HTML tags to add other UI elements;
                    h4("Some other options include..."),                   # <h4> -> 4th level header
                    div("the below inputs are for show only",              # <div style="..."> -> generic 'block' 
                        style = "color: grey; text-decoration: italic;"),  # add style rules for this block (inline css)
                    
                    # Note that there are other options that may work:
                    selectizeInput(
                      "precitors-selectize",
                      "Select predictors... [selectize]",
                      predictors,
                      multiple = TRUE
                    ),
                    
                    # the select and selectize inputs even have a shortcut for
                    # the very common scenario of selecting variable names:
                    varSelectInput(
                      "predictors-var-select",
                      "Select predictors... [var-select]",
                      diamonds %>% select(-price), # we can directly feed in the data, 
                                                   # but we do still need to exclude the outcome variable
                      multiple = TRUE
                    )
                  ),
                  
                  mainPanel(
                    # only show outputs if at least one predictor is selected
                    # note that in javascript <array>.length gives us the length
                    # of an array, in this case the vector of selected predictors.
                    conditionalPanel("input.predictors.length > 0", 
                      plotOutput("plot", height = "800px"),
                      verbatimTextOutput("summary")
                    )
                  )
                )
)

server <- function(input, output) {
  model <- reactive({
    # there is no model if there are no predictors selected
    req(input$predictors)
    
    # we need some way to programmatically alter the linear model. We have two
    # main options here, we could 'pull out' the data column for the predictor
    # and outcome, and run a linear model on that:
    # 
    # .outcome <- diamonds$price
    # .predictor <- diamonds[[input$predictor]]
    # 
    # lm(.outcome ~ .predictor)
    # 
    # This would work, but the output we get now uses '.outcome' and '.predictor'
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
    predictors_formula <- stringr::str_flatten(input$predictors, " + ")
    
    # combine outcome and predictor(s), then create the formula
    formula <- glue::glue("price ~ {predictors_formula}") %>% as.formula()
    
    # note that this is analogous to `lm(price ~ carat + cut, ...)`, but we have
    # to jump through some extra hoops to deal with variable inputs.
    lm(formula, data = diamonds)
  })
  
  output$plot <- renderPlot({
    req(model())
    
    # plot default plot for models, usually some form of fit diagnostic, but
    # it depends on the predictors selected
    par(mfcol = c(2,2)) # plot.lm creates 4 panels for debugging, put them in a 2x2 grid
    model() %>% plot()
  })
  
  output$summary <- renderPrint({ 
    req(model())
    
    model() %>% summary()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
