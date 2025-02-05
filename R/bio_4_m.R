library(terra)
#' bio_1 Create standard deviation of the temperature. 
#' Matrix version.
#'
#' @param tas A matrix with 12 columns of the average monthly temperature.
#'
#' @return a vector with the bio_4 variable
#' @export
#'
#' @examples
bio_4_m <- function(tas){
  fastBioClim::rcpp_parallel_sd(tas)
}


