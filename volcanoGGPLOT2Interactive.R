################################################################################
######################### Volcano Plot with ggplot 2 ###########################
################################################################################

################################################################################
# Author: Priyansh Srivastava ##################################################
# Contact: spriyansh29@gmail.com ###############################################
# Dataset: https://zenodo.org/record/2529117/files/limma-voom_luminalpregnant-luminallactate;
# https://zenodo.org/record/2529117/files/volcano_genes ########################
################################################################################

# Library
library(ggplot2)
library(ggrepel)
library(plotly)
library(tidyverse)
options(ggrepel.max.overlaps = Inf)

# Functions
topGenes <- function(df){
  up <- df %>% slice_min(P.Value, n = 30) %>% slice_max(logFC, n = 10) 
  down <- df %>% slice_min(P.Value, n = 30) %>% slice_min(logFC, n = 10) 
  labels <- rbind(up, down)
  return(labels)
}

# Reading Dataset 
difExpGen <- read.csv("datasets/differentiallyExpressedGenes.csv")

####################
# Data wrangling ###
####################

# Adding empty column
difExpGen$expression <- "Empty"

# if logFc > 0.5 and p-value < 0.05: Over-expressed 
difExpGen$expression[difExpGen$logFC > 0.5 & difExpGen$P.Value < 0.05] <- "Over-expressed"

# if logFc < 0.5 and p-value < 0.05: Under-expressed 
difExpGen$expression[difExpGen$logFC < 0.5 & difExpGen$P.Value < 0.05] <- "Under-expressed"

# if p-value > 0.05: not significant
difExpGen$expression[difExpGen$P.Value > 0.05] <- "Insignificant"

# Reordering the values 
difExpGen$expression <- factor(difExpGen$expression, 
                               levels = c("Over-expressed", 
                                          "Under-expressed",
                                          "Insignificant"))
# label information
geneSymbols <- difExpGen[]

# Okabe-Ito's
pal <- c("#009E73", "#D55E00", "#56B4E9")
pal <- setNames(pal, c("Over-expressed", "Under-expressed", "Insignificant"))

# Designing
volcanoPlotly <- plot_ly(data = difExpGen,
               x = ~logFC, y = ~-log10(P.Value), color = ~expression,
               colors = pal, type = 'scatter', mode = 'markers',
               alpha = 0.4,  hoverinfo = "text",
               text = ~SYMBOL, hovertemplate = paste(
                 "<b>%{text}</b><br>",
                 "%{yaxis.title.text}: %{y}<br>",
                 "%{xaxis.title.text}: %{x}<br>"))

# Correcting axis-labels
volcanoPlotly <- volcanoPlotly %>% layout(xaxis = list(title_text = "logFC"), showlegend = FALSE,
                      yaxis = list(title = "-log10(P.Value)"))

# Removing Display bar
volcanoPlotly <- volcanoPlotly  %>% config(displayModeBar = F)

# Plot
volcanoPlotly