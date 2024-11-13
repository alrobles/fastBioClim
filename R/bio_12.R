#' bio_12 Annual Precipitation Sum
#'
#' @param pr Monthly precipitation raster with 12 layers
#' @param filename the output where to write
#'
#' @return a raster with the bio_12 variable
#' @export
#'
#' @examples
bio_12 <- function(pr, filename = ""){
  terra::app(x = pr, sum, filename = filename)
}


