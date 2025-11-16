#' bio_02  Create mean diurnal temperature range (BIO2)
#'
#' @param tasmax Maximum monthly temperature. A SpatRaster with 12 layers.
#' @param tasmin Minimum monthly temperature. A SpatRaster with 12 layers.
#' @param filename Optional file path to write the output raster.
#' @return A SpatRaster with one layer representing BIO2.
#' @export
#'
#' @examples
#' tmax <- mock_tasmax()
#' tmin <- mock_tasmin()
#' bio_02(tmax, tmin)
bio_02 <- function(tasmax, tasmin, filename = "") {
  # --- Assertions using checkmate ---
  checkmate::assert_class(tasmax, "SpatRaster")
  checkmate::assert_class(tasmin, "SpatRaster")
  checkmate::assert_true(terra::nlyr(tasmax) == 12,
    .var.name = "tasmax must have 12 layers (monthly data)"
  )
  checkmate::assert_true(terra::nlyr(tasmin) == 12,
    .var.name = "tasmin must have 12 layers (monthly data)"
  )
  checkmate::assert_true(all(dim(tasmax)[1:2] == dim(tasmin)[1:2]),
    .var.name = "tasmax and tasmin must have same spatial dimensions"
  )
  checkmate::assert_string(filename, null.ok = TRUE)

  # Create output raster (single layer)
  out <- terra::rast(tasmax, nlyr = 1)
  
  terra::readStart(tasmax)
  terra::readStart(tasmin)
  on.exit(terra::readStop(tasmax))
  on.exit(terra::readStop(tasmin))
  
  ncols <- terra::ncol(tasmax)
  b <- terra::writeStart(out, filename, overwrite = TRUE)
  
  for (i in 1:b$n) {
    v_tasmax <- terra::readValues(tasmax,
                                  row = b$row[i], nrows = b$nrows[i],
                                  col = 1, ncols = ncols, mat = TRUE
    )
    r_tasmax <- parallel_average(v_tasmax)
    
    v_tasmin <- terra::readValues(tasmin,
                                  row = b$row[i], nrows = b$nrows[i],
                                  col = 1, ncols = ncols, mat = TRUE
    )
    r_tasmin <- parallel_average(v_tasmin)
    
    r <- parallel_difference(r_tasmax, r_tasmin)
    terra::writeValues(out, r, b$row[i], b$nrows[i])
  }
  
  terra::writeStop(out)
  names(out) <- "bio_02"
  out
}
