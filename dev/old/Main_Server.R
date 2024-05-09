# Library
library(DT)
library(data.table)
library(limma)
library(tidyverse)
library(dplyr)
library(stringr)
library(Hmisc)
library(plotly)
library(ggplot2)

# Calling Scripts with functions
source("old/serverFunctions/fetchData.R", local = TRUE)
source("old/serverFunctions/degs.R", local = TRUE)

# Server Function
Main_Server <- function(input, output, session) {
  #####################
  ##### Data Ferry ####
  #####################

  # Reactive Values
  values <- reactiveValues(eset = NULL)
  values2 <- reactiveValues(renderedDataset = NULL)

  # fetchEvent
  observeEvent(input$fetch, {
    values$eset <- fetchData(input$goId, input$plId)
  })

  # Display Event (DataFerry)
  observeEvent(input$fetch, {
    values2$renderedDataset <- paste("Showing MetaData for", input$goId, sep = " ")
  })

  # Rendering text for display
  output$renderedDataset <- renderText({
    values2$renderedDataset
  })

  # Rendering table for display (DataFery)
  output$eset <- renderDT({
    datatable(values$eset$disp_p_data, rownames = FALSE, selection = "none")
  })

  # Download for fetched datasets
  output$downData <- downloadHandler(
    filename = function() {
      paste(input$goId, "_", input$downSel, ".csv", sep = "")
    },
    content = function(file) {
      fwrite(as.data.frame(values$eset[[input$downSel]]), row.names = TRUE, sep = ",", file)
    }
  )

  # Tab Switch 1
  observeEvent(input$jumpToDegs, {
    updateTabsetPanel(session, "inTabset", selected = "DEGs")
  })

  # Tab Switch 2
  observeEvent(input$jumpToCoex, {
    updateTabsetPanel(session, "inTabset", selected = "Coex")
  })




  #####################
  ######## DEGs #######
  #####################

  # Reactive Values
  group1 <- reactiveValues(SampleG1 = NULL)
  group2 <- reactiveValues(SampleG2 = NULL)
  clean_df <- reactiveValues(degs = NULL)

  # Rendering table for display (DEGs)
  output$previous <- renderDT({
    datatable(values$eset$disp_p_data, rownames = FALSE)
  })

  # Selection Event (Gorup1)
  observeEvent(input$experiment, {
    group1$G1 <- rownames(values$eset$disp_p_data[as.numeric(input$previous_rows_selected), ])
  })

  # Reset selections
  observeEvent(input$clear, {
    output$previous <- renderDT({
      datatable(values$eset$disp_p_data, rownames = FALSE)
    })
  })

  # Selection Event (Gorup2)
  observeEvent(input$control, {
    group2$G2 <- rownames(values$eset$disp_p_data[as.numeric(input$previous_rows_selected), ])
  })

  # Print Events
  output$SampleG1 <- renderPrint({
    group1$G1
  })
  output$SampleG2 <- renderPrint({
    group2$G2
  })

  # Cleaning Expression Set
  observeEvent(input$analyze, {
    clean_df$degs <- analyze(values$eset$e_data, values$eset$f_data, group1$G1, group2$G2, input$pvalue, input$apvalue)
  })

  # Download Output
  output$donwloadDegs <- downloadHandler(
    filename = function() {
      paste(input$goId, "Differential_Genes", ".csv", sep = "")
    },
    content = function(file) {
      fwrite(clean_df$degs, sep = ",", file, row.names = FALSE)
    }
  )

  # Tab Switch 2
  observeEvent(input$jumpToDegsViz, {
    updateTabsetPanel(session, "degTab", selected = "visualizer1")
  })

  # Rendering table for display (DEGs)
  output$diff_dt <- renderDT({
    datatable(clean_df$degs[, c("Gene.symbol", "ID", "logFC", "adj.P.Val")], rownames = FALSE)
  })

  # Render Volcano Plot
  output$volcano <- renderPlotly({
    make_volcano(clean_df$degs)
  })

  # render Heatmap
  output$heatmap <- renderPlotly({
    make_heatmap(clean_df$degs, values$eset$e_data)
  })


  #####################
  ### Co-Expressded ###
  #####################
}
