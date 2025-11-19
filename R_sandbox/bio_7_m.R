#' Bio 7. Isothermalithy. Difference between
#' Bio 5 and Bio 6
#'
#' @param tasmax Maximum monthly temperature. 
#' @param tasmin Minimum monthly temperature. 
#' @param filename the output filename
#'
#' @return A vector with the range of temperatures
#' @export
#'
#' @examples
bio_7_m <- function(tasmax, tasmin){
  r_1 <- fastBioClim::rcpp_parallel_which_max_row(tasmax)
  r_2 <- fastBioClim::rcpp_parallel_which_min_row(tasmin)
  fastBioClim::rcpp_parallel_difference(r_1, r_2)
}