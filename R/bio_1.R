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
  out <- terra::rast(tas)
  terra::nlyr(out) <- 1
  nc <- terra::ncol(tas)
  terra::readStart(tas)
  on.exit(terra::readStop(tas))
  nl <- terra::nlyr(tas)
  nc <- terra::ncol(tas)
  ncops <- nlyr(tas) / nlyr(out)

  
  b <- terra::writeStart(out, filename, overwrite = TRUE, n = ncops)
  
  for (i in 1:b$n) {
    v <- terra::readValues(tas, b$row[i], b$nrows[i], 1, nc, TRUE)
    r <- fastBioClim::rcpp_parallel_average(v)
    terra::writeValues(out, r, b$row[i], b$nrows[i])
  }
  terra::writeStop(out)
  
}


