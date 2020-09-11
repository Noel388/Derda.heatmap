#' @title Create Heatmap and dendrogram based on the data
#'
#' @description This package creates heatmap and dendrogram based on the experiment data. Meanwhile, clustering based on kmeans and showing the glycan structures.
#'
#' @param name The (list of) intersted glycan(s) name(s).
#' @param show Whether or not print the glycans' names.
#'
#' @return plot
#'
#' @examples
#'
#' @import imager
#'
#' @export show.images
#'
show.images = function(name, show = NULL) {
  len = length(name)
  xy = 1:len
  size = data.frame(xy = xy, x = floor(sqrt(len)))
  size$y = ceiling(with(size, xy / x))
  par(mar = c(1, 1, 1, 1))
  par(mfrow = c(size$x[len], size$y[len]))
  tried = try(
    for (show_i in 1:len) {
      glycan = as.character(name[show_i])
      glycan = get.number(glycan)
      file_name = system.file(paste0("glycans images/", glycan, ".jpg"), package = "Derda.heatmap")
      img = imager::load.image(file_name)
      plot(img, axes = FALSE)
      if (is.null(show)) {
        glycan = get.name(glycan)
        cat(paste0("[", show_i, "]", " ", glycan, "\n"))
      }
    }
  )
  if (is(tried, 'try-error')){
    cat("Error occurs ...\n fixing...\n saving as a pdf named 'Glycans'images.pdf'\n")
    pdf(
      "Glycans' images.pdf",
      width = 30,
      height = 60,
      paper = 'special'
    )
    len = as.numeric(unlist(len))
    par(mar = c(1, 1, 1, 1))
    par(mfrow = c(size$x[len], size$y[len]))
    for (show_i in 1:len) {
      glycan = as.character(name[show_i])
      file_name = system.file(paste0("glycans images/", glycan, ".jpg"), package = "Derda.heatmap")
      img = imager::load.image(file_name)
      plot(img, axes = FALSE)
      if (is.null(show)) {
        cat(paste0("[", show_i, "]", " ", glycan, "\n"))
      }
    }
    dev.off()
  }
}
