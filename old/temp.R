# Load the plotly package
library(plotly)

# Data: mtcars:
data <- as.matrix(mtcars)

# Normalize data
data <- apply(data, 2, function(x){x/mean(x)})

# Heatmap
p <- plot_ly(x=colnames(data), y=rownames(data), 
             z = data, 
             type = "heatmap", 
             colorscale= "Earth",
             showscale = F) %>%
  layout(margin = list(l=120))
p