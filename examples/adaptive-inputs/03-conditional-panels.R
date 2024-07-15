library(shiny)
library(tidyverse)

# Load and pre-process the required data, identical to the previous version.
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

# Define UI for an application that allows the user to select an artist and track,
# and then shows a lineplot of how that track charted over time. The UI is mostly
# identical to the previous version, with one exception...
ui <- fluidPage(titlePanel("2000 top charts"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      "artist",
                      "Select artist...",
                      c(
                        `Select artist to show tracks...` = "",
                        billboard_long %>% pull(artist) %>% unique() %>% sort()
                      )
                    ),
                    # We use `conditionalPanel(...)` to show parts of the UI only when a
                    # certain condition is met. Since this is evaluated in the browser
                    # (as opposed to the R server), the expression should be valid
                    # javascript.
                    #
                    # Shiny makes the `input` and `output` objects available, though we
                    # need to use javascript syntax to access their attributes. Speci-
                    # fically, we need to use `.` instead of `$`.
                    #
                    # We then check to see if `input.artist` is not equal to an empty
                    # string. If this condition is met, the selectInput is shown.
                    conditionalPanel(
                      "input.artist !== ''",
                      # In this case, the track dropdown is the only UI element that is
                      # conditional. We could however make more of the UI conditional,
                      # or even create nested layers of conditional panels.
                      selectInput(
                        "track",
                        "Select track...",
                        billboard_long %>% pull(track) %>% unique() %>% sort()
                      )
                    ),
                    actionButton("do_search", label = "Go!")
                  ),
                  mainPanel(plotOutput("plot"))
                ))

# As conditional panels are evaluated entirely in the browser (i.e., in the ui),
# the server code is identical to the previous step. In more advanced cases how-
# ever, you may want to create outputs solely to be used as a condition for a
# conditional panel.
server <- function(input, output, session) {
  on_artist_change <- observe({
    tracks <-
      billboard_long %>%
      filter(artist == input$artist) %>%
      pull(track) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session,
                      "track",
                      choices = tracks,
                      selected = "")
  }) %>%
    bindEvent(input$artist)
  
  output$plot <- renderPlot({
    ggplot(
      billboard_long %>% filter(artist == input$artist, track == input$track),
      aes(
        x = date,
        y = rank,
        colour = full_title,
        linetype = artist,
        group = full_title
      )
    ) +
      geom_point() +
      geom_line() +
      scale_y_reverse(limits = c(100, 1))
  }) %>%
    bindEvent(input$do_search)
}

# Run the application
shinyApp(ui = ui, server = server)
