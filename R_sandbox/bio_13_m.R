#' bio_13 """Precipitation of wettest month. Matrix
#' version.
#'
#' @param pr A matrix with 12 columns of monthly precipitation
#'
#' @return a vector with the bio_13 variable
#' @export
#'
#' @examples
bio_13_m <- function(pr){
  fastBioClim::rcpp_parallel_which_max_row(pr)
}


