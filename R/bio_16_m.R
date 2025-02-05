#' bio 16 Precipitation of Wettest Quarter   
#' @param pr Monthly precipitation
#' @return A raster with the precipitation of the Wettest Quarter
#' @export
#' @examples
bio_16_m <- function(pr){
  r_1 <- fastBioClim::rcpp_parallel_which_max_quarter(mat_1 = pr[ ,1:(ncol(pr) - 2)],
                                                      mat_2 = pr[ ,2:(ncol(pr) - 1)],
                                                      mat_3 = pr[ ,3:(ncol(pr))])
  fastBioClim::rcpp_get_sum_quarter(quarter = r_1,
                                    mat = pr);
}