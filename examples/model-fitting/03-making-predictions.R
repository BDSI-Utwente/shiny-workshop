library(shiny)
library(tidyverse)

data("diamonds")

# for this example, we assume a fully saturated model, and then let the
# user select values to create a prediction

# the model is static, so define it outside the server
model <- lm(price ~ ., diamonds)

# a helper function to get unique values for a variable in a dataset
choices <- function(.data, variable) {
  .data %>% pull({
    {
      variable
    }
  }) %>% unique() %>% sort() %>% as.character()
}

# helper function to get the median value in an ordinal categorical variable
median <- function(.data) {
  # get counts for each unique value
  counts <- table(.data)
  
  # get middle data point
  mid <- sum(counts) / 2
  
  # take the first unique value that includes the middle data point, then fetch the
  # name for that value
  which(cumsum(counts) >= sum(counts) / 2)[1] %>% names()
}

ui <- fluidPage(titlePanel("Diamond price prediction"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(
                      "carat",
                      "Carats",
                      min = min(diamonds$carat),
                      max = max(diamonds$carat),
                      mean(diamonds$carat),
                      step = 0.1
                    ),
                    selectInput("cut", "Cut", diamonds %>% choices(cut), median(diamonds$cut)),
                    selectInput(
                      "color",
                      "Color",
                      diamonds %>% choices(color),
                      median(diamonds$color)
                    ),
                    selectInput(
                      "clarity",
                      "Clarity",
                      diamonds %>% choices(clarity),
                      median(diamonds$clarity)
                    ),
                    sliderInput(
                      "depth",
                      "Depth",
                      min = min(diamonds$depth),
                      max = max(diamonds$depth),
                      mean(diamonds$depth),
                      step = 0.1
                    ),
                    sliderInput(
                      "table",
                      "Table",
                      min = min(diamonds$table),
                      max = max(diamonds$table),
                      mean(diamonds$table),
                      step = 1
                    ),
                    sliderInput(
                      "x",
                      "Size (X-axis)",
                      min = min(diamonds$x),
                      max = max(diamonds$x),
                      mean(diamonds$x),
                      step = 0.01
                    ),
                    sliderInput(
                      "y",
                      "Size (Y-axis)",
                      min = min(diamonds$y),
                      max = max(diamonds$y),
                      mean(diamonds$y),
                      step = 0.01
                    ),
                    sliderInput(
                      "z",
                      "Size (Z-axis)",
                      min = min(diamonds$z),
                      max = max(diamonds$z),
                      mean(diamonds$z),
                      step = 0.01
                    ),
                    
                  ),
                  
                  mainPanel(textOutput("prediction"))
                ))

server <- function(input, output, session) {
  prediction <- reactive({
    # first we need to create a tibble (data.frame, data_table, etc.) holding
    # our input values.
    
    # I'm going to rely on the fact that I know all the inputs have an id that
    # matches a column in the data to gather the results.
    values <- list()
    # Loop over variable names in the diamonds dataset
    for (name in names(diamonds %>% select(-price))) {
      # Grab input for that variable and store it 
      values[[name]] <- input[[name]]
    }
    
    # generate prediction, with prediction interval
    prediction <-
      predict(model, values %>% as_tibble(), interval = "predict")
    
    # return inputs and prediction
    bind_cols(values, prediction)
  })
  
  output$prediction <- renderText({
    glue::glue(
      "A diamond of {carat} carats, {cut} cut, color {color}, clarity {clarity}, table of {table}, of size {x} by {y} by {z} is worth an estimated {scales::dollar(lwr)} to {scales::dollar(upr)} monopoly bucks.",
      .envir = prediction()
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
