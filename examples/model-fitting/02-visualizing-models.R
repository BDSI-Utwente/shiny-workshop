library(shiny)
library(tidyverse)

data("diamonds")

predictors <- diamonds %>%
  select(-price) %>%
  names()

ui <- fluidPage(titlePanel("Diamond price prediction"),
                sidebarLayout(
                  sidebarPanel(
                    # using the var-selectize input this time
                    
                    # Note that using this input also allows us to use the
                    # order in which the user has selected variables, whereas
                    # checboxGroup input always has a static order.
                    varSelectizeInput(
                      "predictors",
                      "Select predictors...",
                      diamonds %>% select(-price),
                      multiple = TRUE
                    ),
                    
                    # Add additional selector for what to show on x axis
                    varSelectizeInput("x",
                                      "Select x-axis...",
                                      # note that we use an empty vector, we will update choices
                                      # when the user has selected predictors.
                                      c(),
                                      multiple = FALSE)
                  ),
                  
                  mainPanel(fluidRow(
                    column(8, plotOutput("plot")),
                    column(4, verbatimTextOutput("summary"))
                  ))
                ))

server <- function(input, output, session) {
  # Taking a different approach to specifying the model, we're going to filter
  # the data based on user input, then fit the model on all variables.
  # This is especially useful because we can use the filtered dataset to drive
  # the available options for selecting plot parameters in later steps.
  data <- reactive({
    req(input$predictors)
    
    # We're again dealing with non-standard evaluation here, but the `tidy`
    # packages deal with this slightly different than `ggplot`. ggplot came first,
    # but the R language has moved on a bit since then. At some point, ggplot will
    # likely be updated to match the rest of tidyverse (maybe for ggplot3).
    
    # See https://mastering-shiny.org/action-tidy.html for more details, and
    # keep in mind that this is a complex topic, with multiple solutions. Don't
    # worry if you don't fully understand what's happening here.
    
    # We're explicitly selecting our outcome (price), and use the 'splice' operator
    # `!!!` to splice the vector of predictors into individual arguments for the
    # select function.
    diamonds %>% select(price, !!!input$predictors)
  })
  
  # We use an observer to react to any changes in the data, and update the choices
  # available to the user on the UI.
  on_data_changed <- observe({
    req(data())
    
    # note that "data()" is the filtered version created above
    updateVarSelectizeInput(session, "x", 
                            data = data() %>% select(-price),
                            
                            # keep previous selection (what happens if it is now invalid?)
                            selected = input$x)
  }) 
  
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
    # the second when `model()` is updated. Shiny tries to be smart in resolving
    # when to update what so that it has to do as little work as possible, but it's
    # usually better to be explicit.
    bindEvent(data())
  
  output$plot <- renderPlot({
    req(model(), input$x)
    
    # create some temporary data for plotting
    plot_data <- data() %>%
      # attach the predicted values
      mutate(predicted = model()$fitted.values) %>%
      
      # create two datapoints for each observation, one for the observed price,
      # and one for the predicted price. Also adds a new variable "prediction"
      # we can use to distinguish between the two.
      pivot_longer(c(predicted, price),
                   names_to = "prediction",
                   values_to = "price")
    
    plot_data %>%
      ggplot(aes(
        x = !!input$x,
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
