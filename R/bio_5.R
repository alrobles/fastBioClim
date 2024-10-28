#' Bio 5. Max Temperature of Warmest Month
#'
#' @param tasmax Maximum monthly temperature. 
#' @param filename the output filename
#' @return A raster with the maximum value of the monthly temperature time series
#' @export
#'
#' @examples
bio_5 <- function(tasmax, filename = ""){
  out <- rast(tasmax)
  nlyr(out) <- 1
  nc <- ncol(tasmax)
  readStart(tasmax)
  on.exit(readStop(tasmax))
  nl <- nlyr(tasmax)
  nc <- ncol(tasmax)
  ncops <- nlyr(tasmax) / nlyr(out)
  
  b <- writeStart(out, filename, overwrite = TRUE, n=ncops)
  for (i in 1:b$n) {
    v <- readValues(tasmax, b$row[i], b$nrows[i], 1, nc, TRUE)
    r <- fastBioClim::rcpp_parallel_which_max_row(v)
    writeValues(out, r, b$row[i], b$nrows[i])
  }
  writeStop(out)
}