library(terra)
#' bio_1 Create monthly average temperature. Matrix
#' version.
#'
#' @param tas A matrix with 12 columns of the average monthly temperature.
#'
#' @return a vector with the bio_1 variable
#' @export
#'
#' @examples
bio_1_m <- function(tas){
  fastBioClim::rcpp_parallel_average(tas)
}


