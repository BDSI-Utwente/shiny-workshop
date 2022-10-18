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
    date = lubridate::ymd(date.entered) + lubridate::weeks(week),
    week = lubridate::week(date)
  )

# Define UI for an application that allows the user to select an artist and track,
# and then shows a lineplot of how that track charted over time. The UI is identical
# to the previous version.
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
                    selectInput(
                      "track",
                      "Select track...",
                      billboard_long %>% pull(track) %>% unique() %>% sort()
                    ),
                    actionButton("do_search", label = "Go!")
                  ),
                  mainPanel(plotOutput("plot"))
                ))

server <- function(input, output, session) {
  # We want to update the track choices available to the user based on the
  # selected artist. Since we cannot do this before an artist is chosen, we
  # need to react to changes in the selected artist.
  #
  # Note that unlike the `data()` reactive, we do not actually create a value
  # that we will use elsewhere. In that sense, updating the available choices is
  # strictly a side effect. Recall that reactive values have a 'lazy' update
  # strategy - that is, when a change has occured in it's inputs AND the reactive
  # itself or any reactive that depends upon it requests an update.
  #
  # Since we do not return a value, this reactive can have no dependencies, and
  # would therefore never update if we used the 'lazy' `reactive()` function. In
  # contrast, `observe(...)` creates an 'eager' reactive expression that executes
  # whenever any of it's inputs change.
  on_artist_change <- observe({
    # filter the data based on the selected artist, then pull track titles. Note
    # that we use `input$artist`, so we implicitly take a dependency on that input.
    tracks <-
      billboard_long %>%
      filter(artist == input$artist) %>%
      pull(track) %>%
      unique() %>%
      sort()
    
    # shiny provides a number of `updateXxx(...)` functions to update the cor-
    # responding input elements.
    updateSelectInput(session,
                      "track",
                      choices = tracks,
                      selected = "")
  }) %>%
    # Explicitly declare that we take a dependency (only) on `input$artist`.
    # Note that in this case, explicitly declaring our dependencies to be
    # identical to the implicit dependencies is redundant.
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
