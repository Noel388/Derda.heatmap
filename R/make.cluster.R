#' @title Create Heatmap and dendrogram based on the data
#'
#' @description This package creates heatmap and dendrogram based on the experiment data. Meanwhile, clustering based on kmeans and showing the glycan structures.
#'
#' @param data The input experiment data.
#' @param cluster_amount The number of desire clusters.
#'
#' @return kmean cluster
#'
#' @import stats
#'
#' @export make.cluster
#'
make.cluster = function(data, cluster_amount){
  kclus = kmeans(data, cluster_amount)
  return(kclus)
}
