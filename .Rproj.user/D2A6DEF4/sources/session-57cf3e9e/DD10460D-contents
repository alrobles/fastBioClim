#' bio_10 — Mean Temperature of Warmest Quarter (BIO10)
#'
#' Compute **BIO10 (Mean Temperature of Warmest Quarter)** from monthly mean
#' temperature (`tas`) rasters following the CHELSA/WorldClim Bioclimatic
#' Variables framework.
#'
#' @details
#' **BIO10** is the **average monthly mean temperature over the warmest
#' 3‑month period** of the year, computed per cell. This implementation:
#' \enumerate{
#'   \item Identifies, for each cell, the starting month of the **warmest
#'         quarter** by taking rolling 3‑month means over \code{tas}
#'         (optionally allowing wrap‑around windows, e.g., Dec–Jan–Feb when
#'         \code{wrap = TRUE}).
#'   \item Uses that quarter index to compute the **mean of the three monthly
#'         mean temperatures** from \code{tas} for the same months.
#' }
#'
#' The rolling quarter index is obtained with
#' \code{parallel_which_max_quarter(tas_matrix, wrap = ..., na_rm = ...)}.
#' The quarter mean temperature is then computed with
#' \code{parallel_average_quarter(idx, mat, wrap = ..., na_rm = ...)}.
#'
#' @section NA handling:
#' - If \code{na_rm = TRUE}, missing values (\code{NA}) within a 3‑month window
#'   are ignored when computing sums and means. A window with all \code{NA}s
#'   yields \code{NA}.
#' - If \code{na_rm = FALSE} (default), any \code{NA} in a window propagates,
#'   resulting in \code{NA} for that cell.
#'
#' @section Inputs and units:
#' - \code{tas}: monthly mean temperature (°C), 12 layers (Jan–Dec).
#'
#' The raster must be a \code{terra::SpatRaster} with 12 layers. When writing to
#' disk, the output inherits geometry (extent, resolution, CRS) from \code{tas}.
#'
#' @param tas A [terra::SpatRaster] with **12 layers** of monthly mean
#'   temperature (Jan–Dec).
#' @param wrap Logical; if \code{TRUE}, allow wrap‑around rolling quarters
#'   (e.g., Dec–Jan–Feb). If \code{FALSE} (default), only non‑wrapping windows
#'   \code{[1..3], [2..4], ..., [10..12]} are considered.
#' @param filename Optional file path for writing the output raster
#'   (use \code{""} for in‑memory).
#' @param na_rm Logical; if \code{TRUE}, ignore \code{NA}s when selecting and
#'   averaging quarters. If \code{FALSE} (default), any \code{NA} in a window
#'   propagates to the result.
#'
#' @return A [terra::SpatRaster] with one layer representing **BIO10**
#'   (Mean Temperature of Warmest Quarter, units °C).
#'
#' @references
#' Karger, D.N., et al. (2022). CHELSA V2.1: High‑resolution monthly and annual
#' climatologies for the Earth land surface areas. \emph{Earth System Science Data},
#' 14, 5573–5610. \cr
#' Hijmans, R.J., et al. (2005). Very high resolution interpolated climate
#' surfaces for global land areas. \emph{International Journal of Climatology},
#' 25(15), 1965–1978. \cr
#' CHELSA Bioclim dataset overview:
#' \url{https://www.chelsa-climate.org/datasets/chelsa_bioclim}
#'
#' @seealso
#' [parallel_which_max_quarter()], [parallel_average_quarter()]
#'
#' @examples
#' # Mock data (12-layer monthly rasters)
#' tas_ex <- mock_tas()  # mean temperature (°C)
#'
#' # Compute BIO10 without wrap-around (only Jan–Dec contiguous quarters)
#' bio10 <- bio_10(tas_ex, wrap = FALSE)
#' bio10
#'
#' # Allow wrap-around quarters (e.g., Dec–Jan–Feb)
#' bio10_wrap <- bio_10(tas_ex, wrap = TRUE)
#' bio10_wrap
#'
#' # Handle NA values by ignoring them
#' tas_na <- make_1x1_12(c(NA, NA, 30, 1,1,1,1,1,1,1,1,1))
#' bio10_na <- bio_10(tas_na, wrap = FALSE, na_rm = TRUE)
#' bio10_na
#'
#' @export
bio_10 <- function(tas, wrap = FALSE, filename = "", na_rm = FALSE) {
  checkmate::assert_class(tas, "SpatRaster")
  checkmate::assert_true(terra::nlyr(tas) == 12,
                         .var.name = "tas must have 12 layers (monthly data)"
  )
  checkmate::assert_string(filename, null.ok = TRUE)
  
  # Create output raster (single layer)
  out <- terra::rast(tas, nlyr = 1)
  
  terra::readStart(tas)
  on.exit(terra::readStop(tas), add = TRUE)
  
  ncols <- terra::ncol(tas)
  
  b <- terra::writeStart(out, filename, overwrite = TRUE)
  on.exit(try(terra::writeStop(out), silent = TRUE), add = TRUE)
  for (i in 1:b$n) {
    v_tas <- terra::readValues(tas,
                              row = b$row[i], nrows = b$nrows[i],
                              col = 1, ncols = ncols, mat = TRUE
    )
    r_max_tas <- parallel_which_max_quarter(v_tas, wrap = wrap, na_rm = na_rm)
    
    r <- parallel_average_quarter(idx = as.matrix(r_max_tas),
                                  mat =  v_tas,
                                  wrap = wrap, 
                                  na_rm = na_rm)
    
    terra::writeValues(out, r, b$row[i], b$nrows[i])
  }
  
  terra::writeStop(out)
  names(out) <- "bio_10"
  out
}
