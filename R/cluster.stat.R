#' @title Create Heatmap and dendrogram based on the data
#'
#' @description This package creates heatmap and dendrogram based on the experiment data. Meanwhile, clustering based on kmeans and showing the glycan structures.
#'
#' @param data The input experiment data.
#' @param kclus Clusters based on kmean. Should be class of 'kmeans'.
#' @param cluster_num The interested cluster group number.
#' @param stat The type of descriptive statistic: mean, median, or all.
#'
#' @return summary stat
#'
#' @examples
#'
#' @import stats
#'
#' @export cluster.stat
#'
cluster.stat = function(data, kclus, cluster_num, stat = type){
  random_row = sample(1:nrow(data), round(nrow(data)/3), replace = F)
  if (all(names(kclus$cluster[random_row]) == rownames(data)[random_row])){
    if (class(kclus) == "kmeans"){
      if (class(cluster_num) == "numeric"){
        if (any(stat == c("median", "mean", "all"))){
          row_num = which(kclus$cluster == cluster_num)
          target_list = data[row_num,]
          if (stat == "median"){
            cat("Using median as measurement ... \nsorted from the lowest to highest value...")
            sorted_list = sort(apply(target_list, 2, median))
            mini = sorted_list[1]
            maxi = sorted_list[length(sorted_list)]
            cat(paste0("\nExtra info:\nMin. protein is: ", names(mini), " \n-- with (median) measurement of: ", mini))
            cat(paste0("\nMax. protein is: ", names(maxi), " \n-- with (median) measurement of: ", maxi))
            return(sorted_list)
          }
          if (stat == "mean"){
            cat("Using mean as measurement ... \nsorted from the lowest to highest value...")
            sorted_list = sort(apply(target_list, 2, mean))
            mini = sorted_list[1]
            maxi = sorted_list[length(sorted_list)]
            cat(paste0("\nExtra info:\nMin. protein is: ", names(mini), " \n-- with (mean) measurement of: ", mini))
            cat(paste0("\nMax. protein is: ", names(maxi), " \n-- with (mean) measurement of: ", maxi))
            return(sorted_list)
          }
          if (stat == "all"){
            cat("creating all descriptive stat...")
            return(summary(target_list))
          }
        } else{
          stop("please enter a valid stat type: 'median', 'mean', or 'all'")
        }
      } else{
        stop("please enter a valid number. maybe try 'as.numeric()'")
      }
    } else{
      stop("the input kclus is not a result/format of kmean, please first use make.cluster() or kmean() to produce a kmean object.")
    }
  } else{
    stop("please input the same data that produce the clusters.")
  }
}
