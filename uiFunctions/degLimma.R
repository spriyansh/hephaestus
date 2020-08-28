# User Interface for Differential Expression

# Function declaration
findDegs <- function(){
  
  # Tab title declaration
  tabPanel("DEGs", value = "DEGs",
           
           # UI interface
           fluidRow(tags$h2("Diffential Expression Analysis")),
           
           # Side panel
           tabsetPanel(id = "degTab",
                       tabPanel("Set Constraints",br(),
                                
                                # Side panel design
                                sidebarPanel(fluidRow(tags$h3("Control-Panel"), align="center"),br(),br(),
                                             
                                             # Side panel text
                                             fluidRow(tags$h5("Select the samples from the table on the right and assign groups")),
                                             
                                             # Assign Button 1
                                             fluidRow(actionButton("experiment", label = "Assign as 'Experimental'")),br(),
                                             
                                             # Side Panel text 2
                                             fluidRow(tags$h5("Hit 'Reselect' before assigning new group")),
                                             
                                             # Assign Button 2
                                             fluidRow(actionButton("control", label = "Assign as 'Control'")),br(),
                                             
                                             # Set p-Value
                                             fluidRow(numericInput("pvalue", "Set P-Value", 0.03, min = 0.01, max = 0.05)),
                                             
                                             # Set Adjusted P-Value
                                             fluidRow(numericInput("apvalue", "Set Adjusted-P-Value", 0.03, min = 0.01, max = 0.05)),br(),
                                             
                                             # Action button to generate results
                                             fluidRow(actionButton("analyze", "Generate Results")),br(),tags$hr(style="border-color: purple;"),
                                             
                                             # Option 1 for downloading results
                                             fluidRow(column(4,downloadButton("donwloadDegs", "Download Results")), 
                                                      column(4,tags$h5("Or")),
                                                      
                                                      # Option 2 View Results
                                                      column(4,actionButton("jumpToDegsViz", "Visualize Results")), align = "center")),
                                
                                # Main Panel output
                                mainPanel(DT::dataTableOutput("previous"),fluidRow(actionButton("clear", label = "Reselect"),  align = "right"),
                                          
                                          # Showing text selected as Experimental Group
                                          tags$h5("Samples Selected as 'Experiment-Group'"),
                                          
                                          # Dynamic visual
                                          verbatimTextOutput("SampleG1"),br(),
                                          
                                          # Showing text selected as Control Group
                                          tags$h5("Samples Selected as 'Control-Group'"),
                                          
                                          # Dynamic Visual
                                          verbatimTextOutput("SampleG2"),hr())
                       ),
                       
                       # Visual Tab
                       tabPanel("View Results", value = "visualizer1",
                                
                                # DEGs table
                                fluidRow(column(6, DT::dataTableOutput("diff_dt")),
                                         
                                         # volcano plot
                                         column(6,plotlyOutput('volcano'))),hr(),
                                
                                # HeatMap
                                fluidRow(plotlyOutput('heatmap'))
                       )))
  
}