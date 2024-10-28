#' Bio 14. Precipitation of driest month
#'
#' @param pr Yearly precipitation
#' @param filename The output filename
#'
#' @return A raster with the driest month of the year
#' @export
#'
#' @examples
bio_14 <- function(pr, filename = ""){
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
    v_1 <- readValues(pr, b$row[i], b$nrows[i], 1, nc, TRUE)
    r_1 <- fastBioClim::rcpp_parallel_which_min_row(v_1)
    r <- v_1[ ,r_1] 
    writeValues(out, r, b$row[i], b$nrows[i])
  }
  writeStop(out)
}