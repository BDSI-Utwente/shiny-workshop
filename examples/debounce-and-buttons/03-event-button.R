library(shiny)
library(tidyverse)

# As suggested in the previous step, another approach to limiting frequent updates is to
# only 'listen' to specific events. In this example, we add a "Search" button to allow the
# user to manually control when our data is updated.
#
# Of course, we can use the same approach to add buttons to "Update plot", "Fit model",
# "Fetch data", etc., etc. The same approach can also be used to ignore updates in certain
# inputs, intermediate values, etc.
#
# As before, most of the changes occur in the server code. In addition, we add a simple
# search button to the UI.

# Load and pre-process the required data.
data("billboard")
billboard_long <- billboard %>%
    pivot_longer(
        cols = starts_with("wk"),
        names_to = "week",
        names_prefix = "wk",
        values_to = "rank",
        values_drop_na = TRUE
    ) %>%
    mutate(
        full_title = paste(artist, "-", track),
        date = lubridate::ymd(date.entered) + lubridate::weeks(week),
        week = lubridate::week(date)
    )

ui <- fluidPage(
    titlePanel("2000 top charts"),
    sidebarLayout(
        sidebarPanel(
            textInput("search", "Search..."),

            # Add a button with id "do_search" and label "Go!".
            actionButton("do_search", label = "Go!")
        ),
        mainPanel(plotOutput("plot"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # The data() reactive itself is identical to the previous example...
    data <- reactive({
        if (stringr::str_length(input$search) <= 2) {
            return()
        }

        pattern <- stringr::regex(input$search, ignore_case = TRUE)
        billboard_long %>% filter(stringr::str_detect(full_title, pattern))
    }) %>%
        # but we now use `bindEvent(...)` to explicitly tell the reactive what
        # events to listen (bind) to.
        #
        # Note that by default, Shiny creates implicit binds to listen to changes
        # in any reactive values used in an expression. In this case, we would have
        # an implicit bind to `input$search` (the text input).
        #
        # By explicitly binding to `input$do_search` we only update when the user
        # presses the button. Shiny then updates the value of `input$search` if
        # necessary, and updates `data()` with the filtered results.
        bindEvent(input$do_search)

    # Note that we can easily combine binding to specific events with debouncing
    # or throttling. Shiny also provides an easy approach to cache data for specific
    # inputs, see the `bindCache(...)` function.

    output$plot <- renderPlot({
        if (is.null(data())) {
            return()
        }
        ggplot(data(), aes(x = week, y = rank, colour = full_title, linetype = artist, group = full_title)) +
            geom_point() +
            geom_line()
    })
    # Note that as before, limiting updates for `data()` effectively also limits
    # updates for `output$plot`, because `output$plot` has no other reactive inputs.
    # If you want to limit updates for a reactive with multiple inputs, make sure
    # you understand when, how, and how often each input updates.
    #
    # Shiny provides a reactive log to visualize and debug complex reactive depen-
    # dencies. More information on this tool in specific, and debuggin shiny app-
    # lications in general can be found at <https://shiny.rstudio.com/articles/debugging.html>.
}

# Run the application
shinyApp(ui = ui, server = server)
