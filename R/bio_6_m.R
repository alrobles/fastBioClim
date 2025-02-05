#' Bio 6. Min Temperature of Coldest Month
#'
#' @param tasmin Minimum monthly temperature. 
#'
#' @return A vector with the minimum temperature  of coldest month
#' @export
#'
#' @examples
bio_6_m <- function(tasmin){
  fastBioClim::rcpp_parallel_which_min_row(tasmin)
}