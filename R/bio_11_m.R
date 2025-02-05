#' bio 11. Mean Temperature of the coldest Quarter
#'
#' @param tas 
#'
#' @return
#' @export A matrix with the bio_11 var 
#'
#' @examples
bio_11_m <- function(tas){
  r_1 <- fastBioClim::rcpp_parallel_which_min_quarter(mat_1 = tas[ , 1:(ncol(tas) - 2)],
                                                      mat_2 = tas[ , 2:(ncol(tas) - 1)],
                                                      mat_3 = tas[ , 3:(ncol(tas))]  )
  fastBioClim::rcpp_get_avg_quarter(quarter = r_1, mat = tas);
}