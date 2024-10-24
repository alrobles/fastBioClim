#' Bio 5. Min Temperature of Coldest Month
#'
#' @param tasmin Minimum monthly temperature. 
#' @param filename the output filename
#' @return A raster with the minimum value of the monthly minimum temperature time series
#' @export
#'
#' @examples
bio6 <- function(tasmin, filename = ""){
  out <- rast(tasmin)
  nlyr(out) <- 1
  nc <- ncol(tasmin)
  readStart(tasmin)
  on.exit(readStop(tasmin))
  nl <- nlyr(tasmin)
  nc <- ncol(tasmin)
  ncops <- nlyr(tasmin) / nlyr(out)
  
  b <- writeStart(out, filename, overwrite = TRUE, n=ncops)
  for (i in 1:b$n) {
    v <- readValues(tasmin, b$row[i], b$nrows[i], 1, nc, TRUE)
    r <- fastBioClim::rcpp_parallel_which_min_row(v)
    writeValues(out, r, b$row[i], b$nrows[i])
  }
  writeStop(out)
}