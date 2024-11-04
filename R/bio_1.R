#' bio_1 Create monthly average temperature
#'
#' @param tas Average monthly temperature raster with 12 layers
#' @param filename the output where to write
#'
#' @return a raster with the bio_1 variable
#' @export
#'
#' @examples
bio_1 <- function(tas, filename = ""){
  out <- rast(tas)
  nlyr(out) <- 1
  nc <- ncol(tas)
  readStart(tas)
  on.exit(readStop(tas))
  nl <- nlyr(tas)
  nc <- ncol(tas)
  ncops <- nlyr(tas) / nlyr(out)
  
  b <- writeStart(out, filename, overwrite = TRUE, n = ncops)
  
  for (i in 1:b$n) {
    v <- readValues(tas, b$row[i], b$nrows[i], 1, nc, TRUE)
    r <- fastBioClim::rcpp_parallel_average(v)
    writeValues(out, r, b$row[i], b$nrows[i])
  }
  writeStop(out)
  
}


