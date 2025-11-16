#' bio_04  Temperature Seasonality (BIO4)
#'
#' Computes BIO4, **Temperature Seasonality**, from monthly mean temperature
#' rasters following the CHELSA Bioclimatic Variables framework. BIO4 is defined
#' asvthe **standard deviation of monthly mean temperature multiplied by 100**:
#' \deqn{\mathrm{BIO4} = 100 \times
#' \mathrm{sd}(\mathrm{tas}_{\mathrm{Jan..Dec}}).}
#'
#' @details
#' This function implements the calculation of BIO4 as described in:
#' - Karger et al. (2022) "CHELSA V2.1: High-resolution monthly and annual
#'   climatologies for the earth land surface areas".
#'   *Earth System Science Data*, 14, 5573–5610.
#'   <https://essd.copernicus.org/articles/14/5573/2022/>
#' - CHELSA Bioclim dataset:
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
#' @section Units and scaling:
#' Input `tas` is expected in **degrees Celsius** monthly means (Jan–Dec).
#' The output BIO4 is **unitless** by convention because it is `sd(tas) * 100`.
#' If your source rasters are stored in scaled integers (e.g., 10× or 100× °C),
#' ensure they are rescaled to °C before calling this function.
#'
#' @param tas A [terra::SpatRaster] with **12 layers** representing monthly mean
#'   temperature (January–December).
#' @param filename Optional file path to write the output raster.
#'
#' @return A [terra::SpatRaster] with one layer representing **BIO4**
#' (Temperature Seasonality).
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
#' tas_example <- mock_tas()
#' bio_04(tas_example)
bio_04 <- function(tas, filename = "") {
  # --- Assertions using checkmate ---
  checkmate::assert_class(tas, "SpatRaster")
  checkmate::assert_true(
    terra::nlyr(tas) == 12,
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
    # Compute row-wise sample SD across 12 months and scale by 100 (CHELSA BIO4)
    r <- 100 * parallel_sd(v)

    terra::writeValues(out, v = r, b$row[i], b$nrows[i])
  }

  terra::writeStop(out)
  names(out) <- "bio_04"
  out
}
