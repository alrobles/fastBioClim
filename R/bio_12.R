#' bio_12 Annual Precipitation Sum
#'
#' @param pr Monthly precipitation raster with 12 layers
#' @param filename the output where to write
#'
#' @return a raster with the bio_12 variable
#' @export
#'
#' @examples
bio_12 <- function(pr, filename = ""){
  out <- rast(pr)
  nlyr(out) <- 1
  nc <- ncol(pr)
  readStart(pr)
  on.exit(readStop(pr))
  nl <- nlyr(pr)
  nc <- ncol(pr)
  ncops <- nlyr(pr) / nlyr(out)
  
  b <- writeStart(out, filename, overwrite = TRUE, n=ncops)
  
  for (i in 1:b$n) {
    v <- readValues(pr, b$row[i], b$nrows[i], 1, nc, TRUE)
    r <- fastBioClim::rcpp_parallel_cumulative_sum(v)
    writeValues(out, r, b$row[i], b$nrows[i])
  }
  writeStop(out)
  
}


