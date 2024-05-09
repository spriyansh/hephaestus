# Required Libraries
library(GEOquery)
library(genefilter)
library(shinybusy)

#Function to fetch data from Gene Expression Omnibus
fetchData <-function(geo_accession,geo_platform){
  
  # Loading Spinner
  show_modal_spinner()
  
  # Fetching Object
  gset <- getGEO(geo_accession, GSEMatrix =TRUE, AnnotGPL=TRUE)
  
  # Fetching metaData
  if (length(gset) > 1) idx <- grep(geo_platform, attr(gset, "names")) else idx <- 1
  gset <- gset[[idx]]
  fvarLabels(gset) <- make.names(fvarLabels(gset))
  
  # log2 transform
  ex <- exprs(gset)
  qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
  LogC <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
  if (LogC) { ex[which(ex <= 0)] <- NaN
  exprs(gset) <- log2(ex) }
  
  # Converting to df
  p_df <- as.data.frame(pData(gset))
  e_df <- as.data.frame(exprs(gset))
  f_df <- as.data.frame(fData(gset))
  
  # Selecting rows for display
  p_df_display <- p_df[c("title","geo_accession", "characteristics_ch1","source_name_ch1","organism_ch1")]
  
  # Removing Spinner
  remove_modal_spinner()
  
  # Returning pheno Data
  return(list(disp_p_data = p_df_display, e_data = e_df, p_data = p_df, f_data = f_df))
}