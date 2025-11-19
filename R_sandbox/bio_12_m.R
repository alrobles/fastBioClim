library(terra)
#' bio_12 Annual precipitation sum. Matrix
#' version.
#'
#' @param pr A matrix with 12 columns of monthly precipitation
#'
#' @return a vector with the bio_12 variable
#' @export
#'
#' @examples
bio_12_m <- function(pr){
  fastBioClim::rcpp_parallel_cumulative_sum(pr)
}


