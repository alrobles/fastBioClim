#' bio_01  Create annual mean temperature (BIO1)
#'
#' @param tas A SpatRaster with 12 layers (monthly mean temperature).
#' @param filename Optional file path to write the output raster.
#' @return A SpatRaster with one layer representing BIO1.
#' @export
#'
#' @examples
#' tas_example <- mock_tas()
#' bio_01(tas_example)
bio_01 <- function(tas, filename = "") {
  # --- Assertions using checkmate ---
  checkmate::assert_class(tas, "SpatRaster")
  checkmate::assert_true(terra::nlyr(tas) == 12,
                         .var.name = "tas must have 12 layers (monthly data)"
  )
  checkmate::assert_string(filename, null.ok = TRUE)
  
  # Create output raster (single layer)
  out <- terra::rast(tas, nlyr = 1)
  
  # Start reading and writing
  terra::readStart(tas)
  on.exit(terra::readStop(tas))
  
  ncols <- terra::ncol(tas)
  
  b <- terra::writeStart(out, filename, overwrite = TRUE)
  
  for (i in 1:b$n) {
    v <- terra::readValues(
      x = tas,
      row = b$row[i],
      nrows = b$nrows[i],
      col = 1,
      ncols = ncols,
      mat = TRUE
    )
    # Compute row means across 12 months
    r <- parallel_average(v)
    terra::writeValues(out, v = r, b$row[i], b$nrows[i])
  }

  terra::writeStop(out)
  names(out) <- "bio_01"
  out
}
