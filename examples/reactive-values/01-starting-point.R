# load required packages
library(shiny)
library(tidyverse)

# load diamonds dataset
data("diamonds")

# Define UI for application that fits a linear regression to diamond prices by other properties
ui <- fluidPage( # sets up HTML for a 'responsive' app that (should) work on all screen sizes.

  # Application title
  titlePanel("Diamond Dashboard"),

  # sets up HTML for a basic layout with a side 'menu' bar, and a main area for content
  sidebarLayout(

    # declare content for the side bar
    sidebarPanel(

      # create select input (dropdown) for picking predictor property
      # NOTE: the available choices match the variable names in the diamonds data set
      selectInput("variable", "Select variable...", c("carat", "cut", "color", "clarity", "depth", "table"))
    ),

    # declare content for the main panel
    mainPanel(
      # create a responsive layout that tries to fit content side-by-side if the users'
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
      )
    )
  )
)

# Define server logic to draw graph and format summary table
server <- function(input, output) {

  # Note that the elements of `output` match exactly with the names used for the various
  # xxxOutput(...) functions in the UI. Since this data is serialized and sent over a
  # network connection, these names MUST match EXACTLY.
  # The same is true for the elements of the `input` object, defined with the xxxInput(...)
  # functions in the user interface.
  # Both inputs and outputs are reactive values, meaning that if their value changes, any
  # other reactive values that depend on them will automatically be updated in turn.
  # renderPlot(...) automatically captures the output of any plotting function (ggplot2,
  # base R, lattice, etc.). It uses the png device to encode the plot as an image, and
  # then serializes the data to be submitted over the network to the UI.
  output$graph <- renderPlot({
    if (input$variable == "") {
      return() # return null if we haven't selected a variable
    } else {
      # Note that we use the "data pronoun" '.data' here to access a column whose name we
      # do not know a-priori. That is, we select a column with a _variable_ name. Doing so
      # in functions that use 'non-standard evaluation' (e.g., ggplot, tidyverse) requires
      # some additional care.
      # See <https://dplyr.tidyverse.org/articles/programming.html#indirection> for more
      # in-depth discussion about programming with tidyverse functions and variable inputs.
      ggplot(diamonds, aes(x = .data[[input$variable]], y = price)) +
        geom_point() +
        # note that geom_smooth() calculates a fit (a linear model with `method="lm"`) in the
        # background.
        geom_smooth(method = "lm")
    }
  })


  # renderPrint() captures the result of the expression (similar to print()), as well as any
  # output generated (e.g. warning messages, etc.). It then serializes the data, and sends it
  # to the UI.
  output$summary <- renderPrint({
    if (input$variable == "") {
      return() # return null if we haven't selected a variable
    } else {

      # We use `paste(...)` to create a character representation of the formula required as
      # the first argument of the `lm(...) function, which is then parsed as a formula with
      # `as.formula(...)`.
      f <- paste("price", "~", input$variable) %>% as.formula()

      # calculate a linear model, and print the summary.
      lm(f, diamonds) %>% summary()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
