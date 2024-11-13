library(terra)
#' bio_1 Create monthly average temperature
#'
#' @param filename the output where to write
#' @param tasmax Maximum monthly temperature. A set of 12 raster of maximum temperature. 
#' @param tasmin Minimum monthly temperature. A set of 12 raster of minimum temperature.
#'
#' @return a raster with the bio_2 variable
#' @export
#'
#' @examples
bio_2 <- function(tasmax, tasmin, filename = ""){
  out <- rast(tasmax)
  nlyr(out) <- 1
  nc <- ncol(tasmax)
  readStart(tasmax)
  readStart(tasmin)
  on.exit(readStop(tasmax))
  on.exit(readStop(tasmin))
  nl <- nlyr(tasmax)
  nc <- ncol(tasmax)
  ncops <- nlyr(tasmax) / nlyr(out)
  
  b <- writeStart(out, filename, overwrite = TRUE, n=ncops)
  
  for (i in 1:b$n) {
    v_1 <- readValues(tasmax, b$row[i], b$nrows[i], 1, nc, TRUE)
    r_1 <- fastBioClim::rcpp_parallel_average(v_1)
    v_2 <- readValues(tasmin, b$row[i], b$nrows[i], 1, nc, TRUE)
    r_2 <- fastBioClim::rcpp_parallel_average(v_2)
    r <- fastBioClim::rcpp_parallel_difference(r_1, r_2)
    writeValues(out, r, b$row[i], b$nrows[i])
  }
  writeStop(out)
}


