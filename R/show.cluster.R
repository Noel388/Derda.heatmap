#' @title Create Heatmap and dendrogram based on the data
#'
#' @description This package creates heatmap and dendrogram based on the experiment data. Meanwhile, clustering based on kmeans and showing the glycan structures.
#'
#' @param data The input experiment data.
#' @param kclus Clusters based on kmean. Should be class of 'kmeans'.
#' @param cluster_num The interested cluster group number.
#' @param show The number of glycans that want to be printed out. The function will print out all the glycans if not specify.
#' @param show.pic Whether displaying the glycans structure or not.
#'
#' @return list, plot
#'
#' @examples
#'
#' @import stats
#'
#' @export show.cluster
#'
show.cluster = function(data, kclus, cluster_num, show = NULL, show.pic = NULL){
  random_row = sample(1:nrow(data), round(nrow(data)/3), replace = F)
  if (all(names(kclus$cluster[random_row]) == rownames(data)[random_row])){
    if (class(kclus) == "kmeans"){
      if (class(cluster_num) == "numeric"){
        row_num = which(kclus$cluster == cluster_num)
        cluster_list = rownames(data)[row_num]
        if (!is.null(show.pic)){
          show.images(cluster_list, show = T)
        }
        if (is.null(show)){
          return(cluster_list)
        } else{
          return(cluster_list[1:show])
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
