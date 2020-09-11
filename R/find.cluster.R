#' @title Create Heatmap and dendrogram based on the data
#'
#' @description This package creates heatmap and dendrogram based on the experiment data. Meanwhile, clustering based on kmeans and showing the glycan structures.
#'
#' @param kclus Clusters based on kmean. Should be class of 'kmeans'.
#' @param name The interested glycan name.
#'
#' @return number
#'
#' @examples
#'
#' @import stats
#' @import stringr
#'
#' @export find.cluster
#'
find.cluster = function(kclus, name){
  if (class(kclus) == "kmeans"){
    if (class(name) == "character"){
      name = stringr::str_replace_all(name, "−", "-")
      found_row = match(name , names(kclus$cluster))
      if (is.na(found_row)){
        stop("the input name is not found, make sure the input name is valid")
      } else{
        cl = kclus$cluster[found_row]
        print(paste0("cluster is: ", cl))
      }
    }else{
      stop("the input name is not valid, maybe try as.character()?")
    }
  } else{
    stop("the input kclus is not a result/format of kmean, please first use make.cluster() or kmean() to produce a kmean object.")
  }
}
