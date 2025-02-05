library(terra)
#' bio_2  Create mean diurnal temperature range. Matrix 
#' version.
#'
#' @param filename the output where to write
#' @param tasmax A matrix with 12 columns of the maximum monthly temperature.
#' @param tasmin A matrix with 12 columns of the maximum monthly temperature.
#'
#' @return a matrix with the bio_2 variable
#' @export
#'
#' @examples
bio_2_m <- function(tasmax, tasmin){
  r_1 <- fastBioClim::rcpp_parallel_average(tasmax)
  r_2 <- fastBioClim::rcpp_parallel_average(tasmin)
  fastBioClim::rcpp_parallel_difference(r_1, r_2)
  
}


