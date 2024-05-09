################################################################################
############################# Network with D3 ##################################
################################################################################


################################################################################
# Author: Priyansh Srivastava ##################################################
# Contact: spriyansh29@gmail.com ###############################################
# Dataset: https://zenodo.org/record/2529926/files/limma-voom_luminalpregnant-luminallactateDD;
# https://zenodo.org/record/2529926/files/heatmap_genes ########################
# https://zenodo.org/record/2529926/files/limma-voom_luminalpregnant-luminallactate#
################################################################################


# Library
library(igraph)
library(Hmisc)
library(networkD3)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor = (cormat)[ut],
    p = pmat[ut]
  )
}

# Reading Dataset
difExpSet <- read.csv("datasets/heatmapDataset.csv", row.names = 1)

# View
# View(difExpSet)

# Transpose
difExpSet <- t(difExpSet)

# Conversion to matrix
difExpSet <- as.matrix(difExpSet)

# Correlation
corrObj <- rcorr(difExpSet, type = "pearson")

# Conversion to Dataframe
corrleations <- flattenCorrMatrix(corrObj$r, corrObj$P)

# Significant
corrleations <- corrleations[corrleations$p < 0.05, ]

# Correlation based on PC (Upper bound)
corrleations <- corrleations[corrleations$cor > 0.5, ]

# Subsetting to dataframe
links <- corrleations[, c(1, 2)]

# Desigining Network
p <- simpleNetwork(links,
  Source = 1,
  Target = 2,
  opacity = 0.9,
  zoom = T
)

# View
p
