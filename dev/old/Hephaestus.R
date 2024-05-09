# Importing Libraries
library("shiny")
library("shinythemes")
library("shinyWidgets")
#library("BiocManager")
#options(repos = BiocManager::repositories())

# Importing sub-scripts
source('old/Main_UI.R', local = TRUE)
source('old/Main_Server.R', local = TRUE)

# Run-app
shinyApp(
  ui = Main_UI,
  server = Main_Server
)

