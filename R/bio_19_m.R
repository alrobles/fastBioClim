#' bio 19. Precipitation of Coldest Quarter. First we
#' use a mobile window of three months to observer the 
#' quarter of the year (a three month slice) with lower
#' precipitation. After we get the index of this quarter
#' we average the temperature of the three months of 
#' this slice.
#' @param pr Monthly precipitation
#' @param tas Monthly average temperature
#'
#' @return A raster with the mean temperature of the Wettest Quarter
#' @export
#' @examples
bio_19_m <- function(pr, tas){
  r_1 <- fastBioClim::rcpp_parallel_which_min_quarter(mat_1 = tas[ ,1:(ncol(tas) - 2)],
                                                      mat_2 = tas[ ,2:(ncol(tas) - 1)],
                                                      mat_3 = tas[ ,3:(ncol(tas))])
  fastBioClim::rcpp_get_sum_quarter(quarter = r_1, mat = pr);
}