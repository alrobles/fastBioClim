#' bio_02  Mean Diurnal Temperature Range (BIO2)
#'
#' Computes BIO2, the mean diurnal temperature range, from monthly maximum and
#' minimum temperature rasters following the CHELSA Bioclimatic Variables
#' framework.
#'
#' @details
#' BIO2 is defined as the mean of monthly diurnal temperature ranges, where each
#' monthly range is the difference between maximum and minimum temperature.
#' This implementation follows:
#' - Karger et al. (2022) "CHELSA V2.1: High-resolution monthly and annual
#'   climatologies for the earth land surface areas"
#'   *Earth System Science Data*,
#'   14, 5573–5610. <https://essd.copernicus.org/articles/14/5573/2022/>
#' - CHELSA Bioclim dataset:
#' <https://www.chelsa-climate.org/datasets/chelsa_bioclim>
#'
#' The concept of bioclimatic variables originates from:
#' - Nix, H.A. (1986) and was formalized by:
#' - Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., &
#' Jarvis, A. (2005).
#'   "Very high resolution interpolated climate surfaces for global land areas"
#'   *International Journal of Climatology*, 25(15), 1965–1978.
#'   DOI: <https://doi.org/10.1002/joc.1276>
#'
#' @param tasmax A [terra::SpatRaster] with 12 layers representing monthly
#' maximum temperature.
#' @param tasmin A [terra::SpatRaster] with 12 layers representing monthly
#' minimum temperature.
#' @param filename Optional file path to write the output raster.
#'
#' @return A [terra::SpatRaster] with one layer representing BIO2 (mean diurnal
#' temperature range).
#'
#' @references
#' Karger, D.N., et al. (2022). CHELSA V2.1: High-resolution monthly and annual
#' climatologies for the earth land surface areas. *Earth System Science Data*,
#' 14, 5573–5610.
#' Hijmans, R.J., et al. (2005). Very high resolution interpolated climate
#' surfaces for global land areas.
#' *International Journal of Climatology*, 25(15), 1965–1978.
#'
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
  on.exit(try(terra::writeStop(out), silent = TRUE), add = TRUE)

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
