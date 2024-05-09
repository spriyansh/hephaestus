###################
##### Volcano ##@##
###################

###################
#### Libraries ####
###################

# Shiny base
library(shiny)

# Heatmap
library(pheatmap)

# For web app theme
library(shinythemes)

# For simple busy spinner
library(shinybusy)

# For interactive tables
library(DT)


###################
# User-interface ##
###################

# Fluid-page begins; using shinytheme
ui <- fluidPage(
  theme = shinytheme("united"),

  # Nav-page begins
  navbarPage(
    title = "Shiny-Page",

    # Tab-panel begins
    tabPanel("Expression Heatmap", fluidRow(

      # Side bar panel begins
      sidebarPanel(

        # Panel-heading
        fluidRow(tags$h3("Control Panel"), align = "center"), br(), br(),

        # Download button for sample file
        fluidRow(
          tags$h5("Step 0 (Optional): Download a sample file"),

          # Download button UI
          downloadButton("sampleDownload", "Download")
        ), br(),

        # Upload button for sample/User file
        fluidRow(
          tags$h5("Step 1: Upload a sample/custom file"),

          # Upload UI
          fileInput("file1", "Choose CSV File",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
        ),

        # Download
        fluidRow(downloadButton("downloadHeatmap"),
          align = "center"
        ),
      ), # side-panel close

      # main panel begins
      mainPanel(

        # Interactive table display
        DT::dataTableOutput("contents"),

        # Heatmap
        plotOutput("heatmap")
      ) # Main panel close
    )) # Tab-panel close
  ) # nav-bar close
) # fluid-page close

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Sample file Download
  output$sampleDownload <- downloadHandler(

    # Defining file name
    filename = function() {
      "sample.csv"
    },

    # fetch and write the file
    content = function(file) {
      # Loading Spinner
      show_modal_spinner()

      # fetch first write second
      write.csv(
        as.data.frame(read.csv(
          "https://raw.githubusercontent.com/spriyansh/ShinyApps/master/datasets/sample2.csv"
        )), file,
        row.names = FALSE
      )

      # Removing Spinner
      remove_modal_spinner()
    }
  )

  # Reactive upload
  data <- reactive({
    # check-1
    if (!is.null(input$file1)) {
      # Reading file
      read.csv(input$file1$datapath)
    }
  })

  # Display Event
  output$contents <- renderDataTable({
    data()
  })

  # Volcano plot
  output$heatmap <- renderPlot({
    # Check
    req(data())

    # Dynamic input
    df <- data()

    # Making Rownames
    rownames(df) <- df$Id

    # Removing columns
    df <- df[, -1]

    # Making heatmap
    pheatmap(as.matrix(df),
      cellwidth = 80,
      show_rownames = FALSE,
      scale = "row", angle_col = 45
    )
  })

  # Plot volcano
  output$downloadHeatmap <- downloadHandler(

    # filename
    filename = function() {
      "Heatmap.png"
    },

    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # Checks
      req(data())
      df <- data()

      # turn on device
      png(file, height = 17000, width = 30000)
      rownames(df) <- df$Id
      df <- df[, -1]
      pheatmap(as.matrix(df),
        cellwidth = 800, cellheight = 10,
        # show_rownames = FALSE,
        scale = "row"
      )

      # turn off device
      dev.off()
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
