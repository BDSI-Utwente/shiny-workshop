library(shiny)
library(tidyverse)

data("diamonds")

predictors <- diamonds %>%
  select(-price) %>%
  names()

ui <- fluidPage(titlePanel("Diamond price prediction"),
                sidebarLayout(
                  sidebarPanel(
                    # Let user choose predictors
                    selectInput(
                      "predictors",
                      "Select predictors...",
                      diamonds %>% select(-price) %>% names(),
                      multiple = TRUE
                    ),
                    
                    # Add additional selector for which predictor to show on x axis
                    selectInput("x",
                                "Select x-axis...",
                                # note that we start with an empty vector of choices,
                                # we will update choices when the user has selected
                                # one or more predictors.
                                c(),
                                multiple = FALSE)
                  ),
                  
                  mainPanel(plotOutput("plot"),
                            verbatimTextOutput("summary"))
                ))

server <- function(input, output, session) {
  # Taking a different approach to specifying the model, we're going to filter
  # the data based on user input, then fit the model on all variables.
  # This is especially useful because we can use the filtered dataset to drive
  # the available options for selecting plot parameters in later steps.
  data <- reactive({
    req(input$predictors)
    print(input$predictors)
    
    # We're dealing with non-standard evaluation here. Conceptually, the problem
    # is that tidyr, ggplot, and so forth are too smart for their own good,
    # and allow us to pass unquoted column names that they 'magically' match
    # to the context (dataset) they've been given.
    #
    # How can you pass these functions a _variable_ containing the _name(s)_ of
    # the columns that should be selected?
    # The solution is to use some special syntax to make it explicit that the
    # passed variable should be evaluated 'outside' of the data context.
    #
    # To make matters even more confusing, `varSelectInput` returns the selected
    # choice(s) as a (list of) 'symbols', requiring a different syntax to deal
    # with (`!!!` for multiple-choice inputs, `!!` for single choice).
    #
    # See https://mastering-shiny.org/action-tidy.html for more details, and
    # keep in mind that this is a complex topic, with multiple solutions. Don't
    # worry if you don't fully understand what's happening here.
    #
    # To filter the data, we 'manually' select the price column, and use `any_of`
    # to select the remaining columns by name.
    diamonds %>% select(price, any_of(input$predictors))
    
    # if we'd used `varSelectInput`, we'd change the above line to:
    # diamonds %>% select(price, !!!input$predictors)
  })
  
  # We use an observer to react to any changes in the data, and update the choices
  # available to the user on the UI.
  on_data_changed <- observe({
    req(data())
    
    # note that "data()" is the filtered version created above
    updateVarSelectInput(session,
                         "x",
                         data = data() %>% select(-price),
                         
                         # keep previous selection
                         # (note that this may lead to odd results if the previous
                         # selection is no longer valid, but we'll leave that as
                         # an extended homework exercise.)
                         selected = input$x)
  })
  
  # Another reactive expression for the fitted model...
  model <- reactive({
    req(data())
    
    # we can now take the data with selected predictors, and use a 'predict x by all'
    # formula syntax.
    lm(price ~ ., data = data())
  }) %>%
    # update the model after `data()` updates.
    # Note that if we didn't do this, we would create an implicit binding on both
    # `input$predictors` AND the `data()` reactive, potentially causing the model
    # to update twice. The first time immediately after `input$predictors` updates,
    # the second when `data()` is updated. Shiny tries to be smart in resolving
    # when to update what so that it has to do as little work as possible, but it's
    # usually better to be explicit, if only to make our intentions clear to others
    # that read our code (including our future selves!)
    bindEvent(data())
  
  output$plot <- renderPlot({
    req(model(), input$x)
    
    # create some temporary data for plotting, start wit the filtered data
    plot_data <- data() %>%
      # attach the predicted values from the model
      mutate(predicted = model()$fitted.values) %>%
      
      # create two rows for each observation, one for the observed price, and one
      # for the predicted price. Also adds a new variable "prediction" we can use
      # to distinguish between the two.
      pivot_longer(c(predicted, price),
                   names_to = "prediction",
                   values_to = "price")
    
    # plot itself is fairly straightforward, though note that we use the `.data`
    # pronoun to let ggplot know `input$x` is an 'external' variable.
    plot_data %>%
      ggplot(aes(
        x = .data[[input$x]],
        y = price,
        colour = prediction
      )) +
      geom_point(position = "jitter") +
      geom_smooth()
  })
  
  output$summary <- renderPrint({
    req(model())
    
    model() %>% summary()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
