library(shiny)
library(scMaSigPro)
library(shinydashboard)
library(fs)

# UI logic for the R-Info module
mod_ui_env_info_txt <- function(id, col_width = 4) {
    ns <- NS(id)
    column(
        width = col_width,
        textOutput(ns("r_version")),
        shiny::p(""),
        textOutput(ns("package_version")),
        shiny::p(""),
        textOutput(ns("system_info")),
        shiny::p("")
    )
}

# UI logic for the Package Author and Contributors module
mod_ui_auth_cont_info_txt <- function(id, col_width = 4) {
    ns <- NS(id)
    column(
        width = col_width,
        p("Priyansh Srivastava")
    )
}

# UI elements for the R-Info Box
mod_ui_info_box <- function(id, content_ui, box_title = "Box Title", box_width = 6, box_col_width = 6, txt_col_width = 12) {
    ns <- NS(id)
    column(
        width = box_col_width, # Correct variable name usage
        box(
            title = box_title, status = "primary", solidHeader = TRUE, width = box_width,
            content_ui(id, col_width = txt_col_width) # Ensuring correct function callback
        )
    )
}

# UI elements for table
mod_ui_table <- function(id, box_title = "Parameters", box_col_width = 6, box_width = 6) {
    ns <- NS(id)
    column(
        width = box_col_width,
        box(
            title = box_title, status = "primary", solidHeader = TRUE, width = box_width,
            DT::dataTableOutput(ns(id))
        )
    )
}

# UI elements for table
mod_ui_static_plot <- function(id, box_title = "Parameters", box_col_width = 6, box_width = 6) {
    ns <- NS(id)
    column(
        width = box_col_width,
        box(
            title = box_title, status = "primary", solidHeader = TRUE, width = box_width,
            plotOutput(ns(id))
        )
    )
}


# UI elements for MathJax
mod_ui_mathJax <- function(id, box_title = "Parameters", box_col_width = 6, box_width = 6) {
    ns <- NS(id) # Namespace function for ID standardization in Shiny modules
    withMathJax() # Loads MathJax for LaTeX rendering
    
    # Define the column layout
    column(
        width = box_col_width, # Set the width of the column
        box(
            title = box_title, # Set the title of the box
            status = "primary", # Designate the color theme of the box
            solidHeader = TRUE, # Makes the header solid (non-transparent)
            width = box_width, # Set the width of the box
            collapsible = FALSE, # Optional: makes the box collapsible
            uiOutput(ns(id)) # Use namespaced ID for dynamic UI output
        )
    )
}

################################################################################
############### Define Server Modules ##########################################
################################################################################

# Server Logic for the R-Info module
mod_bk_env_info <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        output$r_version <- renderText({
            R.version.string
        })
        output$package_version <- renderText({
            paste("ScMaSigPro", packageVersion("scMaSigPro"), paste0("(", Sys.Date(), ")"))
        })
        output$system_info <- renderText({
            paste("OS", Sys.info()["sysname"])
        })
    })
}

# Server Logic for Parameters Table
mod_table <- function(id, data_frame) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        output[[id]] <- DT::renderDataTable({
            return(data_frame)
        })
    })
}

# Server Logic for Parameters Table
mod_static_plot <- function(id, ggplot_ob) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        output[[id]] <- renderPlot({
            return(ggplot_ob)
        })
    })
}

# Render math jax
mod_mathJax <- function(id, mathJax_txt) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        output[[id]] <- renderUI({
            # Correctly format the LaTeX string for HTML output
            withMathJax(paste0(mathJax_txt))
        })
    })
}

################################################################################
############### getter functions ###############################################
################################################################################

## Function to return Summary
get_analysis_summary <- function(scmp.ob) {
    console_cat_list <- capture.output(scmp.ob)
    
    # Split by the ": "
    split_cat_list <- lapply(strsplit(console_cat_list, ": "), function(x) {
        return(c(x[1], x[2]))
    })
    
    # Convert to frame
    summary_frame <- do.call("rbind", split_cat_list)
    
    # Remove Row 1
    summary_frame <- summary_frame[-1, ]
    
    # Add Column Names
    colnames(summary_frame) <- c("Attribute", "Value")
    
    # Return
    return(summary_frame)
}

## Convert to Mathjax
translate_to_mathjax <- function(formula_string, type ="lm", sep = "\\+") {
    # Split by the "+"
    split_formula <- unlist(str_split(formula_string, pattern = "\\+"))
    
    # Remove any white spaces from any of the elements
    split_formula <- gsub("\\s+", "", split_formula)
    
    # Create "beta0" to "\\beta_{0}"
    split_formula <- gsub("beta0", "\\\beta_{0}", split_formula)
    
    # Replace any underscore that is withing two alphabets by "\_"
    mathjax_formula <- gsub("([a-zA-Z]{2})_([a-zA-Z]{2})", "\\\1\\\\_\\\2", mathjax_formula)
    
    # Add the Mathjax
    mathjax_formula <- paste0("$$", paste(split_formula, collapse = "+"), "$$")
    mathjax_formula
    
    # Return
    return(mathjax_formula)
}


################################################################################
############### APP ############################################################
################################################################################

ui <- dashboardPage(
    dashboardHeader(title = "ScMaSigPro Visualizer"),
    dashboardSidebar(sidebarMenu(
        menuItem("Analysis Summary", tabName = "summary", icon = icon("clipboard-list")),
        menuItem("Additional Information", tabName = "attribute", icon = icon("rectangle-list"))
    )),
    dashboardBody(
        withMathJax(),
        tabItems(
            tabItem(
                tabName = "summary",
                h2("Summary of the Analysis"),
                fluidRow(
                    column(
                        6,
                        mod_ui_table(
                            "analysis_attribute_table",
                            "Analysis Attributes",
                            box_width = 12,
                            box_col_width = 12
                        )
                    ),
                    column(
                        6,
                        mod_ui_table(
                            "parameter_table",
                            "Parameters Table",
                            box_width = 12,
                            box_col_width = 12
                        )
                    )
                ),
                fluidRow(
                    column(6, mod_ui_static_plot(
                        "bin_plot", "Bin Plot",
                        box_width = 12,
                        box_col_width = 12
                    )),
                    column(
                        6,
                        # mod_ui_mathJax(
                        #   id = "mathJax",
                        #   "Polynomial Formula",
                        #   box_width = 12,
                        #   box_col_width = 12
                        # )
                    )
                )
            ),
            tabItem(
                tabName = "attribute",
                h2("Additional Information"),
                fluidRow(
                    mod_ui_info_box(
                        "env_info_box",
                        mod_ui_env_info_txt,
                        "Environment Information",
                        box_width = 12, 6, 6
                    ),
                    mod_ui_info_box(
                        "author_info_box",
                        mod_ui_auth_cont_info_txt,
                        "Author Information",
                        box_width = 12, 6, 6
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {
    mod_bk_env_info("env_info_box")
    mod_table(id = "parameter_table", data_frame = showParams(scmp.ob))
    mod_table(id = "analysis_attribute_table", data_frame = get_analysis_summary(scmp.ob))
    mod_static_plot(id = "bin_plot", ggplot_ob = plotBinTile(scmp.ob))
    # mod_mathJax(id = "mathJax", mathJax_txt = latex_formula)
}

app <- shinyApp(ui, server)
# 
# 
# runApp(app)