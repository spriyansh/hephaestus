# User Interface for Data-Ferry

# Function declaration
dataFerry <- function() {
  # Tab Panel
  tabPanel("Fetch",
    value = "Fetcher", tags$h2("Fetch Datasets from Gene Expression Omnibus (GEO)"), hr(), # tabPanel-2-cont

    # Sidebar Panel
    sidebarPanel(
      fluidRow(tags$h3("Control-Panel"), align = "center"), br(), br(), # sidebarPanel-1-cont

      # Step: 1
      fluidRow(tags$h4("Step-1: Fetch Datasets")),

      # Accession Id Input
      fluidRow(textInput("goId", label = "Enter GEO Accession", value = "GSE65127")), # textInput-1-cont

      # Platform Id Input
      fluidRow(textInput("plId", label = "Enter Platform ID", value = "GPL570")), # textInput-1-cont

      # Action button to Fetch Results
      fluidRow(actionButton("fetch", label = "Fetch")), br(), br(), # sidebarPanel-1-cont

      # Step 2
      fluidRow(tags$h4("Step-2: Download Datasets")),

      # Radio button based choices
      fluidRow(radioButtons("downSel", c("Select the dataset: "),
        choices = c("MetaData" = "p_data", "Annotations" = "f_data", "Expression Counts" = "e_data"),
        selected = "p_data"
      )),

      # Download Action
      fluidRow(downloadButton("downData", "Download")), br(), tags$hr(style = "border-color: purple;"),

      # Link to next page
      fluidRow(column(4, actionButton("jumpToDegs", label = "Differential Genes?")), column(4, tags$h5("Or")),
        column(4, actionButton("jumpToCoex", label = "Co-expressed Genes?")),
        align = "center"
      )
    ), # sidebarPanel-1-close

    # Main Panel Output for Table
    mainPanel(tags$h5("Fetch! to update table"), br(), DT::dataTableOutput("eset"), br(), verbatimTextOutput("renderedDataset")) # mainPanel-1-close
  )
}
