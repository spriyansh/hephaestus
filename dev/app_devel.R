## Load Shiny
library(shiny)

# Source the base UI
ui <- uiOutput("ui")

# Source the base server
server <- function(input, output, session) {
  ui <- reactiveFileReader(1000, session, "dev/app.R", source)
  output$ui <- renderUI(ui())
}

shinyApp(ui = ui, server = server)
