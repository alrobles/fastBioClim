#' bio 8. Mean Temperature of Wettest Quarter. First we
#' use a mobile window of three months to observer the 
#' quarter of the year (a three month slice) with higher
#' precipitation. After we get the index of this quarter
#' we average the temperature of the three months of 
#' this slice. #' 
#'
#' @param pr Monthly precipitation
#' @param tas Monthly average temperature
#'
#' @return A raster with the mean temperature of the Wettest Quarter
#' @export
#' @examples
bio_8_m <- function(pr, tas){
  r_1 <- fastBioClim::rcpp_parallel_which_max_quarter(mat_1 = pr[ ,1:(ncol(pr) - 2)],
                                                        mat_2 = pr[ ,2:(ncol(pr) - 1)],
                                                        mat_3 = pr[ ,3:(ncol(pr))])
  fastBioClim::rcpp_get_avg_quarter(quarter = r_1,
                                    mat = tas);
}