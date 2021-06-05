library(data.table)
library(tidyverse)
library(Hmisc)

# Function for correlation cut-off
corr_cut <- function(temp_df3, cut, cor_index){
  
  # making negative cut-off
  cut_n <- cut * -1
  
  # positive cut-off
  temp_df3_p <- temp_df3[which(temp_df3[,cor_index] > cut),]
  
  # negative cut-off
  temp_df3_n <- temp_df3[which(temp_df3[,cor_index] < cut_n),]
  
  # join 
  joined <- as.data.frame(rbind(temp_df3_p, temp_df3_n))
  
  # Returning the list of 3 DataFrames
  return(joined)
}
  

# Function to convert S3 obj. to matrix
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Function to filter out significant element Significance
aftermath <- function(temp_df2, pvalue, p_index, fdr, adj_index){
  # p-value cut-off
  temp_df2 <- temp_df2[which(temp_df2[,p_index] < as.numeric(pvalue)),]
  
  # FDR Cut-off
  temp_df2 <- temp_df2[which(temp_df2[,adj_index] < as.numeric(fdr)),]
  
  # Returning the list of 3 DataFrames
  return(temp_df2)
}


# Load Data
mat_df <- read.csv("GSE56649_e_data.csv")

# Dropping Nulls
mat_df <- mat_df %>% drop_na()

# Removing Un-annotated Symbols
mat_df <- mat_df[!grepl("NA", mat_df$X),]

# Rownames definition
rownames(mat_df) <- mat_df$X

# Dropping column 1
exprMatr <- mat_df[, -c(1:18)]

# transpose
cor_df <- t(exprMatr)

cor(cor_df,method="pearson")

# Calculation
cor_df  <- rcorr(as.matrix(cor_df, type = "pearson"))

# Extracting columns of interest
cor_df <- as.data.frame(flattenCorrMatrix(cor_df$r, cor_df$P))

# Filtering
cor_df <- corr_cut(cor_df, 0.75, 3)

# Writing
fwrite(cor_df, "co-expressed.csv", sep = ",")

