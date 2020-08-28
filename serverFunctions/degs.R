# Required Libraries
library(data.table)
library(limma)
library(tidyverse)
library(dplyr)
library(stringr)
library(Hmisc)
library(shinybusy)
library(plotly)
library(ggplot2)

# Function to convert List to DataFrame
list_to_df <-function(list1, list2){
  n <- c()
  for(x in list1){
    n<-c(n, x)
  }
  for(y in list2){
    n<-c(n, y)
  }
  n<- as.data.frame(n)
  return(n)
}

# Function to make meta data
meta_make <- function(temp_df, ind_set1,set1, ind_set2, set2){
  s1 <- list(rep(set1,ind_set1))
  s2 <- list(rep(set2,ind_set2))
  g <- list_to_df(s1,s2)
  samp_names <- as.data.frame(colnames(temp_df))
  colnames(samp_names)<- "sample_id"
  temp_meta <- cbind(samp_names, g)
  names(temp_meta)[2]<- "groups"
  return(temp_meta)
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


# Probe Mapping
map_probes <- function(annotated){
  
  # Splitting
  annotated <- as.data.frame(annotated %>% separate_rows(Gene.symbol,Gene.title, Gene.ID, sep = "///"))
  
  # Sorting
  annotated <- annotated[order(annotated$Gene.symbol),]
  
  # Merging
  annotated <- as.data.frame(annotated %>%
                               group_by(Gene.symbol) %>%
                               filter(across(c("logFC"), ~ n_distinct(sign(.)) == 1)) %>%
                               summarise(across(c("logFC","P.Value","adj.P.Val","B","AveExpr","t"), mean), ID = str_c(ID, collapse= " | "),
                                         Gene.title = str_c(Gene.title, collapse= " | "), Gene.ID = str_c(Gene.ID, collapse= " | "),
                                         GenBank.Accession = str_c(GenBank.Accession, collapse= " | ")))
  return(annotated)
}


# Differential expression the dataframe
analyze <- function(df, anno, g1, g2,p,ap){
  
  # Loading Spinner
  show_modal_spinner()
  
  # Subseting Experimental
  df_g1 <- df[, c(g1)]
  
  # Subsetting Control
  df_g2 <- df[, c(g2)]
  
  # Reducing dimensions of dataframe
  df <- as.data.frame(cbind(df_g1, df_g2))
  
  # Drop rows with na
  exp_df <- df[complete.cases(df), ]
  
  # Making groups
  meta_data <- meta_make(exp_df, length(c(g1)), "experiment", length(c(g2)), "control")
  
  # Making factors as labels
  design_factors <- factor(make.names(meta_data$groups))
  
  # Making Design Matrix
  design <- model.matrix(~ design_factors + 0)
  
  # Renaming columns
  colnames(design) <- levels(design_factors)
  
  # Fitting to linear Models
  fit <- lmFit(exp_df, design)
  
  # Making Contrast Matrix
  cont.matrix <- makeContrasts(disease_vs_healthy = experiment-control,levels=design)
  
  # Fitting contrast matrix to linear Models
  fit2  <- contrasts.fit(fit, cont.matrix)
  
  # Emperical Bayes
  fit2  <- eBayes(fit2, 0.01)
  
  # Extracting data
  tT <- as.data.frame(topTable(fit2, adjust="fdr", number = nrow(fit2)))
  
  # Filtering
  stats <- as.data.frame(aftermath(tT, p, 4, ap, 5))
  
  # Cleaning
  stats <- tibble::rownames_to_column(stats, "ID")
  
  # Selecting columns of Interest
  anno <- anno[,c("ID","Gene.symbol","Gene.title","Gene.ID","GenBank.Accession")]
  
  # outer join
  annotate <- merge(stats, anno, by = "ID")
  
  # Remove un-annotated genes
  final_anno <- annotate %>% drop_na()
  
  # Removing Spinner
  remove_modal_spinner()
  
  # Probe Mapping
  df_new <- map_probes(final_anno)
    
  return(df_new)

}

# Function to make reactive Volcano Plot
make_volcano <- function(df_vol){
  
  # Sorting for Insignificant Genes
  df_vol[which(df_vol$logFC < (0.5) | df_vol$logFC > (-0.5)), "colors"] <- "Insignificant"
  
  # Sorting for under-expressed Genes
  df_vol[which(df_vol$logFC < (-0.5)), "colors"] <- "Under-Expressed"
  
  # Sorting for uver-expressed Genes
  df_vol[which(df_vol$logFC > (0.5)), "colors"] <- "Over-Expressed"
  
  # Making ggplot2 object
  volcano <- ggplot(df_vol, aes(x=logFC, y=-log10(P.Value), group=colors, label = "Gene.symbol")) +
    geom_point(aes(color=as.factor(colors)) ,shape=8, alpha = 0.8) +
    theme_bw() + theme(legend.title=element_blank())
  
  # Adding plotly reactivity
  volcano_fig <- ggplotly(volcano, tooltip = "Gene.symbol")
  
  #Passing the plot
  volcano_fig
}

# Function to Make Heatmap
make_heatmap <- function(df, edf){
  
  # Data wrnagling
  e <- tibble::rownames_to_column(edf, "ID")
  
  # Selecting ID
  d <- as.data.frame(df[, c("ID")])
  colnames(d) <- "ID"
  
  # Unfolding the DataFrame
  d <- as.data.frame(d %>% separate_rows(ID, sep = " | "))
  colnames(d) <- "ID"
  
  # Removing pipe strings
  d <- as.data.frame(d[d$ID != "|",])
  colnames(d) <- "ID"
  
  # merge
  h_df <- merge(e, d, by = "ID")
  
  # Savong for plotting
  probes <- h_df$ID
  
  # Converting to DF
  h_df <- as.data.frame(h_df[, -1])
  
  # Matrix conversion
  data <- as.matrix(h_df)
  
  # Heatmap
  htmap <- plot_ly(x=colnames(data), y=probes, z = data, type = "heatmap", 
               colorscale= "YlGnBu", showscale = T) %>% layout(margin = list(l=120))
  
  # Passing Plot
  htmap
  
}