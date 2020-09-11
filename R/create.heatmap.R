#' @title Create Heatmap and dendrogram based on the data
#'
#' @description This package creates heatmap and dendrogram based on the experiment data. Meanwhile, clustering based on kmeans and showing the glycan structures.
#'
#' @param data The input experiment data.
#' @param num_cluster The number of clusters.
#' @param save Whether or not to save the heatmap/dendrogram as a pdf.
#'
#' @return heatmap
#'
#' @examples
#'
#' @import ComplexHeatmap
#' @import circlize
#' @import stats
#'
#' @export create.heatmap
#'
create.heatmap = function(data, num_cluster = NULL, save = NULL){
  if (class(data) == "data.frame" | class(data) == "matrix"){
    col_fun = circlize::colorRamp2(c(-1.6, 0, 25), c("blue", "white", "red"))
    if (!is.null(num_cluster)){
      if (class(num_cluster) == "numeric"){
        if (num_cluster <= nrow(data)){
          kclus = kmeans(data, num_cluster)
          split = paste0("Cluster\n", kclus$cluster)
        } else{
          stop("the input number of clusters is larger than the number of given data, please enter a smaller number (of clusters)")
        }
      }else{
        stop(" the input number of cluster is invalid, please enter a number")
      }
    }else{
      split = NULL
    }
    if (!is.null(save)){
      cat("Creating heatmap pdf... the default name is 'Heatmap & Dendrogram.pdf'\n")
      heat = ComplexHeatmap::Heatmap(data, name = "Heatmap + Dendrogram", col = col_fun, column_title = "Protein",
                     column_title_side = "bottom", row_title = "Glycans", row_dend_side = "right", row_names_gp = grid::gpar(fontsize = 4), column_names_gp = grid::gpar(fontsize = 3), split = split, border = TRUE)
      pdf("Heatmap & Dendrogram111.pdf", width = 30, height = 60, paper='special')
      ComplexHeatmap::draw(heat)
      dev.off()
      cat("Done.")
    } else{
      cat("Creating heatmap...\n")
      heat = ComplexHeatmap::Heatmap(data, name = "Heatmap + Dendrogram", col = col_fun, column_title = "Protein",
                     column_title_side = "bottom", row_title = "Glycans", row_dend_side = "right", row_names_gp = grid::gpar(fontsize = 4), column_names_gp = grid::gpar(fontsize = 3), split = split, border = TRUE)
      ComplexHeatmap::draw(heat)
      cat("Done.")
      cat("\nNote that draw() is required if the heatmap is saved as a variable. i.e., 'draw(variable)' will print the heatmap")
      return(heat)
    }
  } else{
    stop("the input data should be either a dataframe or a matrix")
  }
}
