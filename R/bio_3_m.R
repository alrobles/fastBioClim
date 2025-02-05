#' Bio 3. calculate bio3 "Isothermality"
#' The ratio of diurnal variation (bio2) to annual range of air temperature (bio7).
#' Is multiplied by a factor of 100
#' @param tasmax Maximum monthly temperature. 
#' @param tasmin Minimum monthly temperature. 
#'
#' @return A vector with the range of temperatures
#' @export
#'
#' @examples
bio_3_m <- function(tasmax, tasmin){
  
  r_1 <- fastBioClim::rcpp_parallel_average(tasmax)
  r_2 <- fastBioClim::rcpp_parallel_average(tasmin)
  bio_2 <- fastBioClim::rcpp_parallel_difference(r_1, r_2)
  
  r_1 <- fastBioClim::rcpp_parallel_which_max_row(tasmax)
  r_2 <- fastBioClim::rcpp_parallel_which_min_row(tasmin)
  bio_7 <- fastBioClim::rcpp_parallel_difference(r_1, r_2)
  r <- fastBioClim::rcpp_parallel_quotient(bio_2, bio_7)
  100 * r
}