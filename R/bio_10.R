#' bio 10. Mean Temperature of the warmest Quarter
#'
#' @param tas 
#' @param filename the output where to write
#'
#' @return
#' @export A raster file. 
#'
#' @examples
bio_10 <- function(tas){
  out <- rast(tas);
  nlyr(out) <- 1;
  nc <- ncol(tas);
  readStart(tas);
  on.exit(readStop(tas));
  nl <- nlyr(tas);
  nc <- ncol(tas);
  ncops <- nlyr(tas) / nlyr(out);
  
  b <- writeStart(out, filename, overwrite = TRUE, n = ncops);
  
  for (i in 1:b$n) {
    v_1 <- readValues(tas, b$row[i], b$nrows[i], 1, nc, TRUE)
    r_1 <- fastBioClim::rcpp_parallel_which_max_quarter(mat_1 = v_1[ ,1:(ncol(v_1) - 2)], mat_2 = v_1[ ,2:(ncol(v_1) - 1)], mat_3 = v_1[ , 3:v_1[ ,2:ncol(v_1)]]  )
    #fix name maxQuarter
    r <- fastBioClim::rcpp_get_max_quarter(maxQuarter = r_1, mat = v_1);
    writeValues(out, r, b$row[i], b$nrows[i]);
  }
  
  
}