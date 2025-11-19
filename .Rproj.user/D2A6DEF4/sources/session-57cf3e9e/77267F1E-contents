#' bio_1 Create monthly average temperature
#'
#' @param tas Average monthly temperature raster with 12 layers
#' @param filename the output where to write
#'
#' @return a raster with the bio_1 variable
#' @export
#'
#' @examples
#' 
bio_1 <- function(tas, filename = ""){
    terra::app(x = tas, mean, filename = filename)
}
