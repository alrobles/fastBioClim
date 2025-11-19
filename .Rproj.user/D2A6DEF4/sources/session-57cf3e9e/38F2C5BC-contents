#' bio_05  Max Temperature of Warmest Month (BIO5)
#'
#' Computes BIO5, **Max Temperature of Warmest Month**, from monthly maximum
#' temperature rasters following the CHELSA Bioclimatic Variables framework.
#' For each cell, BIO5 is the **maximum value across the 12 monthly layers**:
#' \deqn{\mathrm{BIO5} = \max(\mathrm{tasmax}_{\mathrm{Jan..Dec}}).}
#'
#' @details
#' This function implements the calculation of BIO5 as described in:
#' - Karger et al. (2022) "CHELSA V2.1: High-resolution monthly and annual
#'   climatologies for the earth land surface areas".
#'   *Earth System Science Data*, 14, 5573–5610.
#'   <https://essd.copernicus.org/articles/14/5573/2022/>
#' - CHELSA Bioclim dataset overview:
#'   <https://www.chelsa-climate.org/datasets/chelsa_bioclim>
#'
#' The concept of bioclimatic variables originates from:
#' - Nix, H.A. (1986) and further formalized by:
#' - Hijmans, R.J.,
#' Cameron, S.E., Parra, J.L., Jones, P.G., & Jarvis, A. (2005).
#' "Very high resolution interpolated climate surfaces for global land areas".
#' *International Journal of Climatology*, 25(15), 1965–1978.
#' DOI: <https://doi.org/10.1002/joc.1276>
#'
#' @section Inputs and units:
#' `tasmax` is expected to be **monthly maximum temperature** in °C
#' (January–December).
#' If the input is scaled (e.g., stored as integers ×10 or ×100),
#' **rescale to °C** prior to calling this function to keep BIO5 in degrees
#' Celsius.
#'
#' @param tasmax A [terra::SpatRaster] with **12 layers** representing monthly
#'   maximum temperature (January–December).
#' @param filename Optional file path to write the output raster.
#'
#' @return A [terra::SpatRaster] with one layer representing **BIO5**
#' (Max Temperature of Warmest Month).
#'
#' @references
#' Karger, D.N., et al. (2022). CHELSA V2.1: High-resolution monthly and annual
#' climatologies for the earth land surface areas. *Earth System Science Data*,
#' 14, 5573–5610.
#' Hijmans, R.J., et al. (2005). Very high resolution interpolated climate
#' surfaces for global land areas. *International Journal of Climatology*,
#' 25(15), 1965–1978.
#'
#' @export
#'
#' @examples
#' # Mock monthly temps.
#' # Replace with tasmax product in practice
#' tasmax_example <- mock_tas()
#' bio_05(tasmax_example)
bio_05 <- function(tasmax, filename = "") {
  # --- Assertions using checkmate ---
  checkmate::assert_class(tasmax, "SpatRaster")
  checkmate::assert_true(
    terra::nlyr(tasmax) == 12,
    .var.name = "tasmax must have 12 layers (monthly data)"
  )
  checkmate::assert_string(filename, null.ok = TRUE)

  # Create output raster (single layer)
  out <- terra::rast(tasmax, nlyr = 1)

  # Start reading and writing
  terra::readStart(tasmax)
  on.exit(terra::readStop(tasmax))

  ncols <- terra::ncol(tasmax)

  b <- terra::writeStart(out, filename, overwrite = TRUE)

  for (i in 1:b$n) {
    v <- terra::readValues(
      x = tasmax,
      row = b$row[i],
      nrows = b$nrows[i],
      col = 1,
      ncols = ncols,
      mat = TRUE
    )
    # Compute row-wise maximum across 12 months (warmest month value per cell)
    r <- parallel_row_max(v)

    terra::writeValues(out, v = r, b$row[i], b$nrows[i])
  }

  terra::writeStop(out)
  names(out) <- "bio_05"
  out
}
