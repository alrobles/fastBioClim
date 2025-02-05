#' bio_14 Precipitation of driest month. Matrix
#' version.
#'
#' @param pr A matrix with 12 columns of monthly precipitation
#'
#' @return a vector with the bio_14 variable
#' @export
#'
#' @examples
bio_14_m <- function(pr){
  fastBioClim::rcpp_parallel_which_min_row(pr)
}


