#' bio_06  Min Temperature of Coldest Month (BIO6)
#'
#' Computes BIO6, **Min Temperature of Coldest Month**, from monthly minimum
#' temperature rasters following the CHELSA Bioclimatic Variables framework.
#' For each cell, BIO6 is the **minimum value across the 12 monthly layers**:
#' \deqn{\mathrm{BIO6} = \min(\mathrm{tasmin}_{\mathrm{Jan..Dec}}).}
#'
#' @details
#' This function implements the calculation of BIO6 as described in:
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
#' `tasmin` is expected to be **monthly minimum temperature** in °C
#' (January–December). If your input is stored as scaled integers (e.g., ×10 or
#' ×100), **rescale to °C** prior to calling this function to keep BIO6 in °C.
#'
#' @param tasmin A [terra::SpatRaster] with **12 layers** representing monthly
#'   minimum temperature (January–December).
#' @param filename Optional file path to write the output raster.
#'
#' @return A [terra::SpatRaster] with one layer representing **BIO6**
#' (Min Temperature of Coldest Month).
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
#' # Replace with tasmin product in practice
#' tasmin_example <- mock_tas()
#' bio_06(tasmin_example)
bio_06 <- function(tasmin, filename = "") {
  # --- Assertions using checkmate ---
  checkmate::assert_class(tasmin, "SpatRaster")
  checkmate::assert_true(
    terra::nlyr(tasmin) == 12,
    .var.name = "tasmin must have 12 layers (monthly data)"
  )
  checkmate::assert_string(filename, null.ok = TRUE)

  # Create output raster (single layer)
  out <- terra::rast(tasmin, nlyr = 1)

  # Start reading and writing
  terra::readStart(tasmin)
  on.exit(terra::readStop(tasmin))

  ncols <- terra::ncol(tasmin)

  b <- terra::writeStart(out, filename, overwrite = TRUE)

  for (i in 1:b$n) {
    v <- terra::readValues(
      x = tasmin,
      row = b$row[i],
      nrows = b$nrows[i],
      col = 1,
      ncols = ncols,
      mat = TRUE
    )
    # Compute row-wise maximum across 12 months (warmest month value per cell)
    r <- parallel_row_min(v)

    terra::writeValues(out, v = r, b$row[i], b$nrows[i])
  }

  terra::writeStop(out)
  names(out) <- "bio_06"
  out
}
