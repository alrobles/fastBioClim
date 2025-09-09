#' bio 16. Precipitation of Wettest Quarter
#'
#' @param pr 
#' @param filename 
#'
#' @return
#' @export A raster file. 
#'
#' @examples
bio_17 <- function(pr, filename = ""){
  out <- rast(pr);
  nlyr(out) <- 1;
  nc <- ncol(pr);
  readStart(pr);
  on.exit(readStop(pr));
  nl <- nlyr(pr);
  nc <- ncol(pr);
  ncops <- nlyr(pr) / nlyr(pr);
  
  b <- writeStart(out, filename, overwrite = TRUE, n = ncops);
  
  for (i in 1:b$n) {
    v_1 <- readValues(pr, b$row[i], b$nrows[i], 1, nc, TRUE)
    r_1 <- fastBioClim::rcpp_parallel_which_min_quarter(mat_1 = v_1[ ,1:(ncol(v_1) - 2)],
                                                        mat_2 = v_1[ ,2:(ncol(v_1) - 1)],
                                                        mat_3 = v_1[ ,3:(ncol(v_1) )]  )
    #fix name maxQuarter
    r <- fastBioClim::rcpp_get_max_quarter(maxQuarter = r_1, mat = v_1);
    writeValues(out, r, b$row[i], b$nrows[i]);
  }
  writeStop(out);
}