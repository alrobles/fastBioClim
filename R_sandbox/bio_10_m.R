#' bio 10. Mean Temperature of the warmest Quarter
#'
#' @param tas 
#'
#' @return
#' @export A matrix with the bio_10 var 
#'
#' @examples
bio_10_m <- function(tas){
  r_1 <- fastBioClim::rcpp_parallel_which_max_quarter(mat_1 = tas[ , 1:(ncol(tas) - 2)],
                                                        mat_2 = tas[ , 2:(ncol(tas) - 1)],
                                                        mat_3 = tas[ , 3:(ncol(tas))]  )
  fastBioClim::rcpp_get_avg_quarter(quarter = r_1, mat = tas);
}