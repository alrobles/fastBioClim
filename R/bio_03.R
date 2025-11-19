#' bio_03  Isothermality (BIO3)
#'
#' Computes BIO3, **Isothermality**, from monthly maximum and minimum
#' temperature rasters following the CHELSA Bioclimatic Variables framework.
#' BIO3 is defined as:
#' \deqn{\mathrm{BIO3} = 100 \times \frac{\mathrm{BIO2}}{\mathrm{BIO7}}}
#' where:
#' \itemize{
#'   \item \strong{BIO2} = Mean Diurnal Temperature Range
#'     (\eqn{\mathrm{mean}_{m}( \mathrm{tasmax}_m - \mathrm{tasmin}_m )})
#'   \item \strong{BIO7} = Temperature Annual Range
#'     (\eqn{\max_m(\mathrm{tasmax}_m) - \min_m(\mathrm{tasmin}_m)})
#' }
#'
#' @details
#' Isothermality compares day–night temperature variability (BIO2) to annual
#' temperature variability (BIO7). The result is expressed as a percentage:
#' \eqn{0–100+}. If BIO7 is zero or near zero, the output is set to `NA` to
#' avoid division by zero.
#'
#' @section Inputs and units:
#' - `tasmax`: monthly maximum temperature (°C), 12 layers (Jan–Dec).
#' - `tasmin`: monthly minimum temperature (°C), 12 layers (Jan–Dec).
#' Both rasters must have identical geometry (extent, resolution, CRS).
#'
#' @param tasmax A [terra::SpatRaster] with **12 layers** representing monthly
#'   maximum temperature.
#' @param tasmin A [terra::SpatRaster] with **12 layers** representing monthly
#'   minimum temperature.
#' @param filename Optional file path to write the output raster
#' ("" for in-memory).
#'
#' @return A [terra::SpatRaster] with one layer representing **BIO3**
#' (Isothermality, in percent).
#'
#' @export
#'
#' @examples
#' tasmax_example <- mock_tas() # mock monthly max temps
#' tasmin_example <- mock_tas() # mock monthly min temps
#' bio_03(tasmax_example, tasmin_example)
bio_03 <- function(tasmax, tasmin, filename = "") {
  # --- Assertions ---
  checkmate::assert_class(tasmax, "SpatRaster")
  checkmate::assert_class(tasmin, "SpatRaster")
  checkmate::assert_true(terra::nlyr(tasmax) == 12,
    .var.name = "tasmax must have 12 layers (monthly data)"
  )
  checkmate::assert_true(terra::nlyr(tasmin) == 12,
    .var.name = "tasmin must have 12 layers (monthly data)"
  )
  # Full geometry check (CRS, extent, resolution, nrow/ncol)
  terra::compareGeom(tasmax, tasmin, stopOnError = TRUE)

  checkmate::assert_string(filename, null.ok = TRUE)

  # Create output raster (single layer), inherit geometry from tasmax
  out <- terra::rast(tasmax, nlyr = 1)

  # --- Start I/O for block processing; avoid double-opening the same source ---
  # Consider both identical object and identical underlying sources
  same_input <- identical(tasmax, tasmin)
  src_max <- tryCatch(terra::sources(tasmax), error = function(e) character())
  src_min <- tryCatch(terra::sources(tasmin), error = function(e) character())
  same_lenght <- length(src_max) == length(src_min)
  same_source <- same_input ||
    (length(src_max) > 0 && same_lenght && all(src_max == src_min))

  terra::readStart(tasmax)
  on.exit(terra::readStop(tasmax), add = TRUE)

  # Check if the second source is the same

  if (!same_source) {
    terra::readStart(tasmin)
    on.exit(terra::readStop(tasmin), add = TRUE)
  }

  # --- Open output for writing; ensure writeStop runs on exit exactly once ---
  ncols <- terra::ncol(tasmax)
  b <- terra::writeStart(out, filename, overwrite = TRUE)
  on.exit(
    {
      # writeStop is safe to attempt at exit; silence any double-close issues
      try(terra::writeStop(out), silent = TRUE)
    },
    add = TRUE
  )

  # --- Numeric guard constants ---
  tol <- 1e-9 # denominator tolerance

  # --- Compute per block ---
  for (i in seq_len(b$n)) {
    # Read a block for all 12 months (mat=TRUE => matrix with 12 columns)
    v_tasmax <- terra::readValues(
      tasmax,
      row = b$row[i], nrows = b$nrows[i],
      col = 1, ncols = ncols, mat = TRUE
    )
    v_tasmin <- terra::readValues(
      tasmin,
      row = b$row[i], nrows = b$nrows[i],
      col = 1, ncols = ncols, mat = TRUE
    )

    # BIO2 (mean diurnal range):
    # average tmax minus average tmin equals average (tmax minus tmin) under a
    # consistent NA policy.
    r_tasmax <- parallel_average(v_tasmax) # row-wise mean over 12 months
    r_tasmin <- parallel_average(v_tasmin)
    # BIO2 proxy
    r_diff <- parallel_difference(r_tasmax, r_tasmin)

    # BIO7 (annual range): max(tmax) - min(tmin)
    r_max_tasmax <- parallel_row_max(v_tasmax) # row-wise max over months
    r_min_tasmin <- parallel_row_min(v_tasmin) # row-wise min over months
    r_maxmin_diff <- parallel_difference(
      as.matrix(r_max_tasmax), # keep 1-col matrix to satisfy C++ function
      as.matrix(r_min_tasmin)
    )

    # BIO3 (%): 100 * BIO2 / BIO7 with division-by-zero guard
    den <- r_maxmin_diff
    num <- r_diff
    den[abs(den) <= tol] <- NA_real_
    r <- 100 * num / den
    r[!is.finite(r)] <- NA_real_

    terra::writeValues(out, r, b$row[i], b$nrows[i])
  }

  # Explicit close (also guarded by on.exit if an error happened earlier)
  terra::writeStop(out)
  names(out) <- "bio_03" # Isothermality (%)
  out
}
