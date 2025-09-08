# Mean Temperature of the driest Quarter
#

#' bio 9. Mean Temperature of driest Quarter. First we
#' use a mobile window of three months to observer the 
#' quarter of the year (a three month slice) with lower
#' precipitation. After we get the index of this quarter
#' we average the temperature of the three months of 
#' this slice. #' 
#'
#' @param pr Monthly precipitation
#' @param tas Monthly average temperature
#' @param filename the output where to write
#'
#' @return A raster with the mean temperature of the driest Quarter
#' @export
#'
#' @examples
bio_9 <- function(pr, tas, filename = ""){
  out <- rast(pr);
  nlyr(out) <- 1;
  nc <- ncol(pr);
  readStart(pr);
  on.exit(readStop(pr));
  readStart(tas);
  on.exit(readStop(tas));
  nl <- nlyr(pr);
  nc <- ncol(pr);
  ncops <- nlyr(pr) / nlyr(out);
  
  b <- writeStart(out, filename, overwrite = TRUE, n = ncops);
  for (i in 1:b$n) {
    v_1 <- readValues(pr, b$row[i], b$nrows[i], 1, nc, TRUE)
    r_1 <- fastBioClim::rcpp_parallel_which_min_quarter(mat_1 = v_1[ ,1:(ncol(v_1) - 2)], mat_2 = v_1[ ,2:(ncol(v_1) - 1)], mat_3 = v_1[ , 3:v_1[ ,2:ncol(v_1)]]  )
    v_2 <- readValues(tas, b$row[i], b$nrows[i], 1, nc, TRUE);
    #fix name maxQuarter
    r <- fastBioClim::rcpp_get_max_quarter(maxQuarter = r_1, mat = v_2);
    writeValues(out, r, b$row[i], b$nrows[i]);
  }
  writeStop(out);
}