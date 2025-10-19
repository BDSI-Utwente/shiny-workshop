
# This app will work fine, but it refreshes the data and plot after each update
# to the search query - potentially multiple types per second, depending on typing
# speed.
# Depending on the performance of the R server, speed and latency of the connection
# the server and user, and the performance of the users' device, the user will exper-
# ience lag, and the user interface may become unresponsive. The R server is also
# needlessly stressed, potentially requiring a more powerful (and more expensive)
# machine, or slowing down interactions with other users.
# Ideally, we would like to control when and limit how often the search results are
# updated. The following two examples show two approaches to solving this problem.

library(shiny)
library(tidyverse)

# Load and pre-process the required data.
data("billboard")
billboard_long <- billboard %>%
    # The billboard dataset is a textbook example of a 'wide' dataset, we use
    # the example code from the `pivot_wider(...)` function to create a 'long'
    # dataset containing one row for each track, for each week that it listed
    # in the charts.
    pivot_longer(
        cols = starts_with("wk"),
        names_to = "week",
        names_prefix = "wk",
        values_to = "rank",
        values_drop_na = TRUE
    ) %>%
    # We create a 'full_title' field to aid searching, and do some clean-
    # up of the date fields.
    mutate(
        full_title = paste(artist, "-", track),
        week = parse_integer(week),
        date = lubridate::ymd(date.entered) + lubridate::weeks(week)
    )

ui <- fluidPage(
    # Application title
    titlePanel("2000 top charts"),

    # Create an extremely simple sidebar layout with a search field in the side
    # bar, and a graph of chart placements over time for matching tracks on the
    # right.
    sidebarLayout(
        sidebarPanel(
            textInput("search", "Search...")
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input, output) {
    # create a reactive filtering the 'raw' data based on the users' search query.
    data <- reactive({
        # to avoid getting too many (meaningless) matches, only search when the
        # query is at least two characters long.
        if (stringr::str_length(input$search) <= 2) {
            return()
        }

        # create a case insensitive regex pattern based on the query
        pattern <- stringr::regex(input$search, ignore_case = TRUE)

        # filter tracks that match the query (recall that full_title) combines
        # artist and track, so we can match in both.
        billboard_long %>% filter(stringr::str_detect(full_title, pattern))
    })

    # draw a line plot of track chart positions over time.
    output$plot <- renderPlot({
        if (is.null(data())) {
            return()
        }
        ggplot(data(), aes(x = date, y = rank, colour = full_title, linetype = artist, group = full_title)) +
            geom_point() +
            geom_line()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
