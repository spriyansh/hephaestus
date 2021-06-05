###################
##### Volcano ##@##
###################

###################
#### Libraries ####
###################

# Shiny base
library(shiny)

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
ui <- fluidPage(theme = shinytheme("united"),
                
                # Nav-page begins
                navbarPage(title = "Shiny-Page",
                           
                           # Tab-panel begins
                           tabPanel("Volcano plot",fluidRow(
                                    
                                    # Side bar panel begins
                                    sidebarPanel(
                                      
                                      # Panel-heading
                                      fluidRow(tags$h3("Control Panel"), align = "center"), br(), br(),
                                      
                                      # Download button for sample file
                                      fluidRow(tags$h5("Step 0 (Optional): Download a sample file"),
                                               
                                               # Download button UI
                                               downloadButton("sampleDownload", "Download")), br(),
                                      
                                      # Upload button for sample/User file
                                      fluidRow(tags$h5("Step 1: Upload a sample/custom file"),
                                               
                                               # Upload UI
                                               fileInput("file1", "Choose CSV File",
                                                         multiple = FALSE,
                                                         accept = c("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".csv"))),
                                      
                                      # Slider for Adjusted P-value
                                      fluidRow(tags$h5("Step 2: Adjust p-value"),
                                               sliderInput("pValue", "Slide to adjust",
                                                           min = 0.01, max = 0.1, value = 0.05)),
                                      
                                      # Slider for Fold change
                                      fluidRow(tags$h5("Step 3: Adjust Fold change"),
                                               sliderInput("fC", "Slide to adjust",
                                                           min = 0, max = 1.5, value = 1)),
                                      
                                      # Download 
                                      fluidRow(downloadButton("downloadVolcano"), 
                                               align = "center"),
                                      
                                    ), # side-panel close
                                    
                                    # main panel begins
                                    mainPanel(
                                      
                                      # Interactive table display
                                      DT::dataTableOutput("contents"),
                                      
                                      # Number of DEGs
                                      tags$h6("Number of differential genes"),
                                      verbatimTextOutput("degs")
                                      
                                    ) # Main panel close   
                           )), # Tab-panel close
                           fluidRow(
                             plotOutput("volcano")
                           ) # fluidRow closed
                ) # nav-bar close
) # fluid-page close

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Sample file Download
  output$sampleDownload <- downloadHandler(
    
    # Defining file name
    filename = function() {"sample.csv"},
    
    # fetch and write the file
    content = function(file) {
      
      # Loading Spinner
      show_modal_spinner()
      
      # fetch first write second
      write.csv(as.data.frame(read.csv(
        "https://raw.githubusercontent.com/spriyansh/ShinyApps/master/All_list_fix.csv",
        sep = "\t")), file, row.names = FALSE)
      
      # Removing Spinner
      remove_modal_spinner()
      
    })
  
  # Reactive upload 
  data <- reactive({
    
    # check-1
    if(!is.null(input$file1)){
      
      # Reading file
      read.csv(input$file1$datapath)    
    }
  })

  # Display Event
  output$contents <- renderDataTable({data()})
    
  # Volcano plot
  output$volcano <- renderPlot({
    
    # Check
    req(data())

    # Dynamic input
    df <- data()
    
    # p-values and log fold change
    sig <- df[df$adj.P.Val <= input$pValue & abs(df$logFC) >= input$fC, ]
    
    # Plotting the volcano
    plot(df$logFC, -log10(df$adj.P.Val), pch="*", 
         
         # Reactive title
         main = paste("Volcano Plot (FC: ",input$fC, "; P-value: ", input$pValue, ")"),
         
         xlab="Log2 Fold Change", ylab = "-10log (adjusted p-value)")
    
    # reference lines
    abline(h=-log10(input$pValue), v=c(-input$fC, input$fC), col="red", lty=2)
    
    # Points
    points(sig$logFC, -log10(sig$adj.P.Val), col="red", pch="*")
    
  })
  
  # Reactive printing
  output$degs <- renderPrint({
    
    # Check
    req(data())
    
    # Dynamic input
    df <- data()
    
    # Number of differential Genes
    n_degs <- nrow(as.data.frame(df[df$adj.P.Val <= as.numeric(input$pValue) & abs(df$logFC) >= as.numeric(input$fC), ]))
    
    # printing the output
    print(n_degs)
    
  })
  
  # Plot volcano
  output$downloadVolcano <- downloadHandler(
    
    # filename
    filename =  function() {"Volcano.png"},
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      
      # Checks
      req(data())
      df <- data()
      
      # turn on device
      png(file)
      
      # Plotting
      sig <- df[df$adj.P.Val <= input$pValue & abs(df$logFC) >= input$fC, ]
      plot(df$logFC, -log10(df$adj.P.Val), pch="*", 
      main = paste("Volcano Plot (FC: ",input$fC, "; P-value: ", input$pValue, ")"),
      xlab="Log2 Fold Change", ylab = "-10log (adjusted p-value)")
      abline(h=-log10(input$pValue), v=c(-input$fC, input$fC), col="red", lty=2)
      points(sig$logFC, -log10(sig$adj.P.Val), col="red", pch="*")
      
      # turn the device off
      dev.off()
      } )
}

# Run the application 
shinyApp(ui = ui, server = server)