#' bio 17 Precipitation of driest Quarter   
#' @param pr Monthly precipitation
#' @return A matrix with the precipitation of the driest
#' @export
#' @examples
bio_17_m <- function(pr){
  r_1 <- fastBioClim::rcpp_parallel_which_min_quarter(mat_1 = pr[ ,1:(ncol(pr) - 2)],
                                                      mat_2 = pr[ ,2:(ncol(pr) - 1)],
                                                      mat_3 = pr[ ,3:(ncol(pr))])
  fastBioClim::rcpp_get_sum_quarter(quarter = r_1,
                                    mat = pr);
}