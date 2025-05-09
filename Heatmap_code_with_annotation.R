library(ComplexHeatmap)
library(tidyverse)

random_test <- read.delim("random_test.txt")

# set gene names as row names
random_test <- column_to_rownames(random_test, var = "Gene_names") 

## split file between log2 and padj
log2_fc <- as.matrix(random_test[,c(1:4)])
padj_fc <- as.matrix(random_test[,c(5:8)])

## annotation for cut out
custom_annotation <- function(j, i, x, y, width, height, fill) {
  grid::grid.polygon(x=unit.c(x-width/2, x-width/2, x-width/10),
                     y=unit.c(y-width/-25, y-height/2, y-height/2),
                     gp=grid::gpar(fill="black", col=NA))
}

# function to draw triangle in the cell
cell_function_triangle = function(j, i, x, y, width, height, fill) {
  if(is.na(padj_fc[i, j])) {
  } else if(padj_fc[i, j] <= 0.05){
    custom_annotation(j, i, x, y, width, height, fill)
  }
}

## color for heatmap
paletteLength <- 1000
myColour <- colorRampPalette(c( "green", "white","red"))(paletteLength) 

## heatmap code
ComplexHeatmap::pheatmap(log2_fc, na_col="grey", cellheight = 10,
                cellwidth = 20, fontsize_row = 8, cluster_rows = FALSE,
                cluster_cols = FALSE, scale = "none", color = myColour,
                column_title = "Title",
                row_names_side = "left", column_names_side = "top",
                heatmap_legend_param = list(at = c(0, 5, 10), title=
                      "level",
                      legend_height = unit(4, "cm"))
)



## add triangle annotation to heatmap , if padj < 0.05, then draw triangle in the cell using cell_fun of complex heatmap


## heatmap code with triangle annotation
ComplexHeatmap::pheatmap(log2_fc, na_col="grey", cellheight = 10,
               cellwidth = 20, fontsize_row = 8, cluster_rows = FALSE,
               cluster_cols = FALSE, scale = "none", color = myColour,
               column_title = "Title",
               row_names_side = "left", column_names_side = "top",
               cell_fun = cell_function_triangle,
               heatmap_legend_param = list(at = c(0, 5, 10), title=
                        "level",
                        legend_height = unit(4, "cm"))
)


