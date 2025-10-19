library(shiny)
library(tidyverse)

# Two common approaches to limit the effect of rapidly chanding inputs are
# debouncing and throttling. Both are similar in that they limit the number
# of times an reactive is executed, but they differ in when that execution
# happens.
#
# When we debounce an expression, we wait a given amount of time after each
# change in inputs before we execute the expression. If another change occurs
# during this debounce period, the waiting time is restarted.
#
# Consider the below schematic. In the events row, an 'x' notes an event (e.g.,
# change in input). In the debounce row, a '.' denotes the debounce timer (5
# 'units', or 5 dots), and x denotes the debounced expression updating when
# no futher events are triggered for 5 seconds. Note that there were 8 events,
# yet the expression only updated 3 times.
#
# events:   x   x           x x x           x   x  x
# debounce: .........x      .........x      ............x
#
# Throttling is similar in concept, but instead executes immediately after the
# first event, and subsequently limits the number of updates for repeated events
# over time. With a limit of 1 update every 5 'ticks', the schematic for throttling
# is shown below.
#
# events:   x   x           x x x           x   x  x
# debounce: .........x      .........x      ............x
# throttle: x....           x....           x....  x....
#
# Note that throttling leads to more updates than debouncing, but reduces the
# maximum lag between a change in input and the resulting output update. With
# throttling, we respond to the leading event, whereas with debouncing, we respond
# to the trailing event. In both cases, we can optionally add additional extra
# updates, guaranteeing a maximum lag time between events and updates.
#
# events:    x   x           x x x           x   x  x
# debounce:  .........x      .........x      ............x
# debounce+: ....x....x      ....x....x      ....x....x..x
#
# events:    x   x           x x x           x   x  x
# throttle:  x....           x....           x....  x....
# throttle+: x....x          x....x          x....x....x
#
# Which of these strategies is optimal, and how long the timeout period should be
# depends on the interaction (e.g. debouncing is common for search fields, throttling
# is more appropriate for dropdowns), the importance of consistency (how quickly
# does the UI need to update), and the cost of the update - updating a simple UI
# element is going to be much quicker than re-fitting a complex model.
#
# Enough about the theory, let's implement it. The only changes required are in the
# R server code, so the data preparation and UI code are identical to the previous
# version.

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
        week = parse_integer(week),
        date = lubridate::ymd(date.entered) + lubridate::weeks(week)
    )

ui <- fluidPage(
    titlePanel("2000 top charts"),
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
    data <- reactive({
        if (stringr::str_length(input$search) <= 2) {
            return()
        }

        pattern <- stringr::regex(input$search, ignore_case = TRUE)

        billboard_long %>% filter(stringr::str_detect(full_title, pattern))
    }) %>% debounce(1000)
    # Piping the reactive into the debounce (or throttle) function is all we
    # need to do. In this case, we use a debounce strategy with a timeout of 1
    # second (1000 milliseconds). This is a rather long timeout chosen to highlight
    # the effect, you'll probably want to use something shorter.

    output$plot <- renderPlot({
        if (is.null(data())) {
            return()
        }
        ggplot(data(), aes(x = date, y = rank, colour = full_title, linetype = artist, group = full_title)) +
            geom_point() +
            geom_line()
    })
    # Note that debouncing `data()` effectively also debounces `output$plot`,
    # because `output$plot` has no other reactive inputs. If the plot depended
    # on multiple inputs, we may want to debounce rendering the plot itself
    # instead of (or in addition to) the inputs, or introduce and debounce a new
    # intermediate reactive value to combine both inputs.
    #
    # Alternatively, we could explicitly limit the inputs that a reactive value
    # 'listens' to for updates. This option is explained more in the next step
    # of this example.
}

# Run the application
shinyApp(ui = ui, server = server)
