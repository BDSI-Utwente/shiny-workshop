#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hello World!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("name", "What is your name?")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("welcome")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$welcome <- renderText({
        if (input$name == ""){
            return("Well met, stranger")
        }
        
        paste0("Hi there, ", input$name, "!")
    })
}

# Run the application 
shinyApp(ui = ui, server = server,
         options = list(display.mode = "showcase"))

