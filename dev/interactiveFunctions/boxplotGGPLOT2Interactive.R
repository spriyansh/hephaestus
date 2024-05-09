################################################################################
############################ Boxplot with plotly ###############################
################################################################################


################################################################################
# Author: Priyansh Srivastava ##################################################
# Contact: spriyansh29@gmail.com ###############################################
# Dataset: https://zenodo.org/record/2529926/files/limma-voom_luminalpregnant-luminallactateDD;
# https://zenodo.org/record/2529926/files/heatmap_genes ########################
# https://zenodo.org/record/2529926/files/limma-voom_luminalpregnant-luminallactate#
################################################################################


# Library
library(ggplot2)
library(plotly)
options(ggrepel.max.overlaps = Inf)

# Reading Dataset
longExpSet <- read.csv("datasets/boxDataset.csv")

# Making color palette
pal <- c("#009E73", "#D55E00")

# Designing plot
interactiveBoix <- plot_ly(longExpSet,
  x = ~Sample, y = ~Expression, color = ~Group, type = "box",
  colors = pal
)
# Hiding the bar
interactiveBoix <- interactiveBoix %>% config(displayModeBar = F)

interactiveBoix
