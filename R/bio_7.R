#' Bio 7.  "annual range of air temperature"
#' The difference between the Maximum Temperature of Warmest month and the Minimum Temperature of Coldest month
#'
#' @param tasmax Maximum monthly temperature. 
#' @param tasmin Minimum monthly temperature. 
#' @param filename the output where to write
#'
#' @return A raster with the range of temperature
#' @export
#'
#' @examples
bio_7 <- function(tasmax, tasmin, filename = ""){
  out <- rast(tasmax)
  nlyr(out) <- 1
  nc <- ncol(tasmax)
  readStart(tasmax)
  on.exit(readStop(tasmax))
  readStart(tasmin)
  on.exit(readStop(tasmin))
  nl <- nlyr(tasmax)
  nc <- ncol(tasmax)
  ncops <- nlyr(tasmax) / nlyr(out)
  
  b <- writeStart(out, filename, overwrite = TRUE, n=ncops)
  for (i in 1:b$n) {
    v_1 <- readValues(tasmax, b$row[i], b$nrows[i], 1, nc, TRUE)
    r_1 <- fastBioClim::rcpp_parallel_which_max_row(v_1)
    
    v_2 <- readValues(tasmin, b$row[i], b$nrows[i], 1, nc, TRUE)
    r_2 <- fastBioClim::rcpp_parallel_which_min_row(v_2)
    
    r <- fastBioClim::rcpp_parallel_difference(r_1,  r_2)
    writeValues(out, r, b$row[i], b$nrows[i])
  }
  writeStop(out)
}