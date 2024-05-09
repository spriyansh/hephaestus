# Loading Libraries
library("shiny")
library("shinythemes")
library("DT")
library("plotly")



# Calling Scripts with functions
source("old/uiFunctions/dataFerry.R", local = TRUE)
source("old/uiFunctions/degLimma.R", local = TRUE)

Main_UI <- shinyUI({
  fluidPage(
    theme = shinytheme("flatly"), # fluidPage-cont
    navbarPage(
      title = "Hephaestus", id = "inTabset", # navbarPage-cont
      tabPanel("Home"), # tabPanel-1-close
      dataFerry(), # tabPanel-2-close
      findDegs(), # tabPanel-3-close
      tabPanel("Co-Expressed", value = "Coex"), # tabPanel-4-close
      tabPanel("Contact") # tabPanel-5-close
    ) # navbarPage-close
  ) # fluidPage-close
}) # function-close
