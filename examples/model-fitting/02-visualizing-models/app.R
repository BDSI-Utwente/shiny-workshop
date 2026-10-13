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
    # We're dealing with non-standard evaluation here. Conceptually, the problem
    # is that tidyr, ggplot, and so forth are too smart for their own good,
    # and allow us to pass unquoted column names that they 'magically' match
    # to the context (dataset) they've been given.
    #
    # For example, consider the following code:
    # diamonds %>% select(price, carat)
    # 
    # We're passing 'price' and 'carat' as unquoted column names, how does R know
    # these are actually names of variables in the dataset, and not objects in the
    # global environment? If we tried this with most other functions, R would throw 
    # an error. Instead, tidyverse functions use 'non-standard evaluation' to infer
    # that these are column names, and look them up in the dataset.
    # 
    # How can you pass these functions a _variable_ containing the _name(s)_ of
    # the columns that should be selected? If we just pass the variable, it would
    # be interpreted as a column name, not the value of the variable. 
    # The solution is to use some special syntax to make it explicit that the
    # passed variable should be evaluated 'outside' of the data context. We use the 
    # '.data' and '.env' pronouns to make this explicit for single variable names, 
    # any_of() [or its sibling all_of()] for a vector of one or more variable names.
    # See https://mastering-shiny.org/action-tidy.html
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
    # note that "data()" refers to the reactive expression created above
    req(data())
    
    # names of selected predictors
    predictor_names <- names(data() %>% select(-price))
    updateSelectInput(session,
                      "x",
                      choices = predictor_names,
                      
                      # maintain selection if it's still valid, otherwise clear it
                      selected = if_else(input$x %in% predictor_names, input$x, NA_character_))
  })
  
  # Another reactive expression for the fitted model...
  model <- reactive({
    # data is available and predictor(s) have been selected
    req(data(), input$predictors)
    
    # we can now take the data with selected predictors, and use a 'predict x by all'
    # formula syntax.
    lm(price ~ ., data = data())
  }) 
  
  output$plot <- renderPlot({
    # model was fitted, and a valid x-axis variable has been selected
    req(model(), input$x, input$x %in% names(data()))
    
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
    # pronoun to let ggplot know `input$x` is a variable whose value is the name of 
    # the column to use for the x-axis.
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
    # basic model summary only depends on the model itself
    req(model())
    
    model() %>% summary()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
