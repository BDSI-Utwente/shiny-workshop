library(shiny)
library(tidyverse)
library(DALEX)

#**
#* We've developed a model predicting diamond prices, and would like to share that with
#* the world. This example implements a simple Shiny app that allows the user to fill
#* in diamond properties and show the predicted price in real time.
#*
#* Our advanced AI model is trained on the 'diamonds' dataset included with the ggplot2
#* package, using state-of-the-art linear regression techniques implemented in the lm
#* package.
#* 
#* First, we'll quickly go over how we would normally implement this functionality in a 
#* 'static' R script. 
#**

# prepare dataset, remove the x,y,z dimensions to simplify things a bit (this info is
# also largely captured in gem weight (carat)).
data <- diamonds |>
  select(-(x:z))

# fit the model
model <- lm(price ~ ., data = data)

# we can then make predictions using new data...
new_data <- tibble_row(
  carat = 2,
  cut = "Good",
  color = "E",
  clarity = "VVS2",
  depth = 61.4,
  table = 56
)
predict(model, new_data)


# below we'll implement an app that has the minimal user interface required to let a
# user input diamond values, and present the calculated price.

ui <- fluidPage(
  h1("Gem Prize Calculator™"),
  wellPanel(
    # note that Shiny (via package htmlTools) provides R wrappers around (almost) all
    # HTML elements, making it easy to create basic HTML structures. In this case we 
    # create a paragraph (<p>), containing some italic (emphasized, <em>) text.
    p(
      "Welcome to our", em("\"totally reliable\""), " Gem Prize Calculator™.",
      "Input the details of your gems below, click 'Calculate!', and our advanced
         AI will calculate the prize of your gems in a fraction of a second!"
    ),

    # create appropriate inputs, dropdowns for the ordinal predictors, numeric input
    # fields for continuous predictors. Note the different step values to control the 
    # precision of inputs on the UI. 
    numericInput("carat", "Carat", data$carat |> mean() |> round(1), step = 0.01),
    selectInput("cut", "Cut", data$cut |> levels()),
    selectInput("color", "Color", data$color |> levels()),
    selectInput("clarity", "Clarity", data$clarity |> levels()),
    numericInput("depth", "Depth", data$depth |> mean() |> round(), step = 0.1),
    numericInput("table", "Table", data$table |> mean() |> round(), step = 1),

    # we'll hide the predicted prize until the user clicks a button
    actionButton("predict", "Calculate!", class = "btn-primary")
  ),

  # use a conditional panel to hide/show UI elements based on inputs/outputs
  conditionalPanel(
    # note that the condition is evaluated in the browser, using javascript syntax.
    # Shiny makes the input and output objects available, but we'll need to 
    # use javascript syntax to access the elements on these objects. Usually, that
    # just means using "." instead of "$", but for advanced cases there might be 
    # some more caveats. 
    "input.predict",
    wellPanel(
      # again using R wrappers for HTML header (2nd level, <h2>) and 'span' of text 
      # (<span>) elements. We also apply some custom CSS styling to the span element 
      # to make the price stand out more. 
      h2("Your gem is worth..."),
      span(textOutput("prediction"), style = "font-size: 280%; font-family: 'Comic Sans MS', sans-serif;"),
    )
  )
)

server <- function(input, output, session) {
  # on the server side, we collect the user inputs, make a prediction, and format it 
  # for rendering on the UI. 

  output$prediction <- renderText({
    # create a one-row tibble of data to make predictions for
    new_data <- tibble_row(
      carat = input$carat,
      cut = input$cut,
      color = input$color,
      clarity = input$clarity,
      depth = input$depth,
      table = input$table
    )

    # make prediction using fitted model, the scales package provides
    # various helper functions for labelling graph axis scales, but they
    # can also be used to quickly format numbers as currency, percentages,
    # etc.
    predict(model, new_data) %>%
      scales::dollar()
  })
}

shinyApp(ui, server, options = list(test.mode = TRUE))
