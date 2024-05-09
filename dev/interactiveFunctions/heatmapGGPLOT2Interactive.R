################################################################################
############################ Heatmap with ggplot 2 #############################
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
library(reshape2)
library(viridis)
library(plotly)
library(heatmaply)

# Reading Dataset
difExpSet <- read.csv("datasets/heatmapDataset.csv", row.names = 1)

# View
# View(difExpSet)

hMap <- heatmaply(as.matrix(difExpSet),
  dendrogram = "column",
  xlab = "Samples", ylab = "Genes",
  margins = c(1, 1, 1, 1),
  grid_color = "white",
  grid_width = 0.00001,
  branches_lwd = 0.1,
  labCol = colnames(difExpSet),
  labRow = rownames(difExpSet),
  heatmap_layers = theme(
    axis.line = element_blank(),
    custom_hovertext = NULL
  )
)
hmap
