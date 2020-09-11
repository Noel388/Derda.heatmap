#' @title Create Heatmap and dendrogram based on the data
#'
#' @description This package creates heatmap and dendrogram based on the experiment data. Meanwhile, clustering based on kmeans and showing the glycan structures.
#'
#' @param data The input experiment data.
#' @param kclus Clusters based on kmean. Should be class of 'kmeans'.
#' @param cluster_num The interested cluster groups' numbers.
#' @param protein The specific protein of interest.
#'
#' @return table
#'
#' @examples
#'
#' @import stats
#'
#' @export compare.cluster
#'
compare.cluster = function(data, kclus, cluster_nums, protein = NULL){
  random_row = sample(1:nrow(data), round(nrow(data)/3), replace = F)
  if (all(names(kclus$cluster[random_row]) == rownames(data)[random_row])){
    if (class(kclus) == "kmeans"){
      if (class(cluster_nums) == "numeric"){
        all_summary = c()
        # there are two types - specify protein or not
        if (is.null(protein)){
          count = 1
          cat("Calculating summary statistics for all proteins ...")
          for (cluster_i in 1:length(cluster_nums)){
            cluster_num = cluster_nums[cluster_i]
            row_num = which(kclus$cluster == cluster_num)
            cluster_list = data[row_num,]
            all_summary = rbind(all_summary, summary(cluster_list), "")
            rownames(all_summary)[count] = paste0("cluster_", cluster_num)
            count = count + 7
          }
          cat("\nDone.\n")
          return(all_summary)
        } else{
          if (class(protein) == "character"){
            if (any(which(colnames(data) == protein))){
              cat(paste0("Calculating summary statistics for the protein ", protein, " ..."))
              for (cluster_i in 1:length(cluster_nums)){
                cluster_num = cluster_nums[cluster_i]
                row_num = which(kclus$cluster == cluster_num)
                cluster_list = data[row_num,]
                protein_col = which(colnames(cluster_list) == protein)
                cluster_sum = summary(cluster_list[,protein_col])
                all_summary = cbind(all_summary, cluster_sum)
              }
              colnames(all_summary) = paste0("cluster_", cluster_nums)
              cat("\nDone.\n")
              return(all_summary)
            } else{
              stop("invalid protein name, not found in the dataset")
            }
          } else{
            stop("invalid protein name, or try as.character()")
          }
        }
      } else{
        stop("please enter a valid number list. Ex. c(1,3,5)")
      }
    } else{
      stop("the input kclus is not a result/format of kmean, please first use make.cluster() or kmean() to produce a kmean object.")
    }
  } else{
    stop("please input the same data that produce the clusters.")
  }
}
