#' bio_07  Temperature Annual Range (BIO7)
#'
#' Computes BIO7, **Temperature Annual Range**, from monthly maximum and minimum
#' temperature rasters following the CHELSA Bioclimatic Variables framework.
#' BIO7 is defined as:
#' \deqn{\mathrm{BIO7} = \max(\mathrm{tasmax}_{\mathrm{Jan..Dec}}) -
#'       \min(\mathrm{tasmin}_{\mathrm{Jan..Dec}}).}
#'
#' @details
#' This function implements the calculation of BIO7 as described in:
#' - Karger et al. (2022) "CHELSA V2.1: High-resolution monthly and annual
#'   climatologies for the earth land surface areas".
#'   *Earth System Science Data*, 14, 5573–5610.
#'   <https://essd.copernicus.org/articles/14/5573/2022/>
#' - CHELSA Bioclim dataset overview:
#'   <https://www.chelsa-climate.org/datasets/chelsa_bioclim>
#'
#' The concept of bioclimatic variables originates from:
#' - Nix, H.A. (1986); further formalized by:
#' - Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., & Jarvis, A.
#' (2005).
#'   "Very high resolution interpolated climate surfaces for global land areas".
#'   *International Journal of Climatology*, 25(15), 1965–1978.
#'   DOI: <https://doi.org/10.1002/joc.1276>
#'
#' @section Inputs and units:
#' - `tasmax`: monthly maximum temperature (°C), 12 layers (Jan–Dec).
#' - `tasmin`: monthly minimum temperature (°C), 12 layers (Jan–Dec).
#' Both rasters must have identical spatial dimensions. If stored as scaled
#' integers (e.g., ×10 or ×100), **rescale to °C** before calling this function.
#'
#' @param tasmax A [terra::SpatRaster] with **12 layers** representing monthly
#'   maximum temperature.
#' @param tasmin A [terra::SpatRaster] with **12 layers** representing monthly
#'   minimum temperature.
#' @param filename Optional file path to write the output raster.
#'
#' @return A [terra::SpatRaster] with one layer representing **BIO7**
#' (Temperature Annual Range).
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
#' tasmax_example <- mock_tas() # mock monthly max temps
#' tasmin_example <- mock_tas() # mock monthly min temps
#' bio_07(tasmax_example, tasmin_example)
bio_07 <- function(tasmax, tasmin, filename = "") {
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
  on.exit(terra::readStop(tasmax), add = TRUE)
  on.exit(terra::readStop(tasmin), add = TRUE)
  
  ncols <- terra::ncol(tasmax)

  b <- terra::writeStart(out, filename, overwrite = TRUE)
  on.exit(try(terra::writeStop(out), silent = TRUE), add = TRUE)
  
  for (i in 1:b$n) {
    v_tasmax <- terra::readValues(tasmax,
      row = b$row[i], nrows = b$nrows[i],
      col = 1, ncols = ncols, mat = TRUE
    )
    r_max_tasmax <- parallel_row_max(v_tasmax)


    v_tasmin <- terra::readValues(tasmin,
      row = b$row[i], nrows = b$nrows[i],
      col = 1, ncols = ncols, mat = TRUE
    )
    r_min_tasmin <- parallel_row_min(v_tasmin)

    r_maxmin_diff <- parallel_difference(
      as.matrix(r_max_tasmax),
      as.matrix(r_min_tasmin)
    )

    terra::writeValues(out, r_maxmin_diff, b$row[i], b$nrows[i])
  }

  terra::writeStop(out)
  names(out) <- "bio_07"
  out
}
