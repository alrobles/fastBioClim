#' Bio 5. Max Temperature of Warmest Month
#'
#' @param tasmax Maximum monthly temperature. 
#'
#' @return A vector with the range of temperatures
#' @export
#'
#' @examples
bio_5_m <- function(tasmax){
  fastBioClim::rcpp_parallel_which_max_row(tasmax)
}