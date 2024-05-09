################################################################################
############################ MA Plot with ggplot 2 #############################
################################################################################

################################################################################
# Author: Priyansh Srivastava ##################################################
# Contact: spriyansh29@gmail.com ###############################################
# Dataset: https://zenodo.org/record/2529117/files/limma-voom_luminalpregnant-luminallactate;
# https://zenodo.org/record/2529117/files/volcano_genes ########################
################################################################################

# Library
library(plotly)

# Reading Dataset 
difExpGen <- read.csv("datasets/differentiallyExpressedGenes.csv")

# Okabe-Ito's
pal <- "#56B4E9"

# Designing
maPlotly <- plot_ly(data = difExpGen,
                         x = ~AveExpr, y = ~logFC, 
                         colors = pal, type = 'scatter', mode = 'markers',
                         alpha = 0.4,  hoverinfo = "text",
                         text = ~SYMBOL, hovertemplate = paste(
                           "<b>%{text}</b><br>",
                           "%{yaxis.title.text}: %{y}<br>",
                           "%{xaxis.title.text}: %{x}<br>",
                           "<extra></extra>"))

# Correcting axis-labels
maPlotly <- maPlotly %>% layout(xaxis = list(title_text = "AveExpr"), 
                                showlegend = FALSE,
                                yaxis = list(title = "logFC"))

# Removing Display bar
maPlotly <- maPlotly  %>% config(displayModeBar = F)

# Plot
maPlotly