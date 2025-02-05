#' bio_15 Precipitation Seasonality  
#' version.
#'
#' @param pr A matrix with 12 columns of monthly precipitation
#'
#' @return a vector with the bio_15 variable
#' @export
#'
#' @examples
bio_15_m <- function(pr){
  fastBioClim::rcpp_parallel_coef_var(pr)
}


