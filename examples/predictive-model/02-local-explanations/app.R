library(shiny)
library(tidyverse)
library(DALEX)

#**
#* Following up from the baseline app, we've found that our users don't necessarily trust
#* our advanced AI model, and want to know _why_ it makes the predictions that it does.
#*
#* Let's add that functionality using the 'DALEX' package.
#* Note that this is trivial for linear regression, as this can be directly calculated
#* from the observed parameters and model coefficients. The feature importance given in
#* DALEX and other similar packages are primarily used to explain predictions from complex
#* AI models that may otherwise be hard to interpret. The approach is the same, so we'll
#* use the simple case as an example.
#**

# prepare data and model as before
data <- diamonds |>
  select(-(x:z))
model <- lm(price ~ ., data = data)

# prepare an 'explainer' for the model/data
explainer <- explain(model,
  data = data %>% select(-price),
  y = data$price
)

# we can use the explainer to get various measures of variable importance. In this case,
# we're primarily interested in how any given prediction is calculated
new_data <- data %>% sample_n(1) # grab a random gem

# get parameter influence on final price prediction, from most influential to least
# influential.
prediction <- predict(model, new_data)
prediction
influence <- predict_parts(explainer, new_data)
influence
plot(influence)

# User interface is mostly identical to the baseline app...
ui <- fluidPage(
  h1("Gem Prize Calculator™"),
  wellPanel(
    p(
      "Welcome to our", em("\"totally reliable\""), " Gem Prize Calculator™.",
      "Input the details of your gems below, click 'Calculate!', and our advanced
         AI will calculate the prize of your gems in a fraction of a second!"
    ),
    numericInput("carat", "Carat", data$carat |> mean() |> round(1), step = 0.01),
    selectInput("cut", "Cut", data$cut |> levels()),
    selectInput("color", "Color", data$color |> levels()),
    selectInput("clarity", "Clarity", data$clarity |> levels()),
    numericInput("depth", "Depth", data$depth |> mean() |> round(), step = 0.1),
    numericInput("table", "Table", data$table |> mean() |> round(), step = 1),
    actionButton("predict", "Calculate!", class = "btn-primary")
  ),
  conditionalPanel(
    # we're also taking some more control about how the user goes through the app,
    # using specific outputs to show/hide the conditional panels. These outputs 
    # are logical (TRUE/FALSE) in R, but transmitted to the UI as text, so we'll 
    # need to add quotation marks and explicitly check for the "TRUE" string.
    "output.show_prediction == 'TRUE'",
    wellPanel(
      h2("Your gem is worth..."),
      span(textOutput("prediction"), style = "font-size: 280%; font-family: 'Comic Sans MS', sans-serif;"),

      # add another button to conditionally show vip plot
      actionButton("explain", "Explain calculation")
    )
  ),

  conditionalPanel(
    "output.show_explanation == 'TRUE'",
    wellPanel(
      plotOutput("explanation_plot")
    )
  )
)

server <- function(input, output, session) {
  # because we're now using the user inputs in multiple places, we'll
  # separate it into it's own reactive expression.
  values <- reactive({
    tibble_row(
      carat = input$carat,
      cut = input$cut,
      color = input$color,
      clarity = input$clarity,
      depth = input$depth,
      table = input$table
    )
  })

  # simple text output - same as baseline
  output$prediction <- renderText({
    # note that the 'values' reactive containing user inputs is a _function_
    # that we have to call instead of simply reference ('values()' vs 'values')
    predict(model, values()) %>%
      scales::dollar()
  })

  # plot output showing contributions of each input
  output$explanation_plot <- renderPlot({
    predict_parts(explainer, values()) |> plot()
  })

  # implement some user interface interactions; 
  #  - only show predictions and explanations when the corresponding 
  #    button is pressed.
  #  - hide prediction/explanation when any input is changed
  # 
  # One way to implement this (there are others, including using 
  # uiOutput/renderUI) is to create toggle or 'flag' outputs for 
  # conditionalPanels that we can control from the server function. 
  # 
  # We'll use reactiveValues to create a list of reactive values, 
  # and initialize toggles for showing the prediction and explanation
  # as FALSE. 
  # 
  # Note that controlling user interaction is not necessary for the 
  # basic functioning of this app (and may actually make it worse), 
  # but it is extremely important for more complex apps in real 
  # world scenarios. 
  toggles <- reactiveValues(
    show_prediction = FALSE,
    show_explanation = FALSE
  )

  # Create an observer that resets the toggles to FALSE whenever 
  # any input changes. We use bindEvent to make the observer 
  # trigger whenever a(ny) given reactive(s) changes. 
  on_inputs_changed <- observe({
    toggles$show_prediction <- FALSE
    toggles$show_explanation <- FALSE
  }) %>% bindEvent(values())

  # Create two observers that set the appropriate toggle when a 
  # button is clicked. 
  on_predict <- observe({
    toggles$show_prediction <- TRUE
  }) %>% bindEvent(input$predict)

  on_explain <- observe({
    toggles$show_explanation <- TRUE
  }) %>% bindEvent(input$explain)

  # Create outputs to communicate the toggles to the UI. Note that 
  # the logical value in the toggle is sent to the UI as a string, 
  # so we'll need to add appropriate quatation marks when using it.
  output$show_prediction <- renderText(toggles$show_prediction)
  output$show_explanation <- renderText(toggles$show_explanation)
  
  # By default, shiny is lazy - meaning it only updates reactives 
  # when they are visible on the UI. As we will only use these 
  # toggles in the condition for conditionalPanels, we'll need to 
  # let shiny know these outputs need to be updated even when it 
  # thinks they are not shown.
  outputOptions(output, "show_prediction", suspendWhenHidden = FALSE)
  outputOptions(output, "show_explanation", suspendWhenHidden = FALSE)
}

shinyApp(ui, server, options = list(test.mode = TRUE))
