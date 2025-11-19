#' bio_08  Mean Temperature of Wettest Quarter (BIO8)
#'
#' Computes **BIO8 (Mean Temperature of Wettest Quarter)** from monthly
#' precipitation (`pr`) and monthly mean temperature (`tas`) rasters following
#' the CHELSA/WorldClim Bioclimatic Variables framework.
#'
#' @details
#' BIO8 is defined as the **average monthly mean temperature over the wettest
#' 3‑month period** of the year, computed per cell. This implementation:
#' \enumerate{
#'   \item Finds, for each cell, the starting month of the **wettest quarter**
#'         by taking rolling 3‑month precipitation sums (optionally with
#'         wrap‑around windows, e.g. Dec–Jan–Feb when \code{wrap = TRUE}).
#'   \item Uses that quarter index to compute the **mean of the three monthly
#'         mean temperatures** from \code{tas} for the same months.
#' }
#'
#' The rolling quarter index is obtained with
#' \code{parallel_which_max_quarter(pr_matrix, wrap = ...)}, and the
#' quarter mean temperature is computed with
#' \code{get_average_quarter(idx, tas_matrix)} which averages
#' over the three months (skipping \code{NA}s among those three values; all
#' \code{NA} in the window yields \code{NA}).
#'
#' @section Inputs and units:
#' - `pr`: monthly total precipitation (e.g., mm), 12 layers (Jan–Dec).
#' - `tas`: monthly mean temperature (°C), 12 layers (Jan–Dec).
#' Both rasters must have identical spatial geometry (extent, resolution, CRS)
#' and the same number of rows/columns.
#'
#' @param pr A [terra::SpatRaster] with **12 layers** representing monthly
#'   total precipitation.
#' @param tas A [terra::SpatRaster] with **12 layers** representing monthly
#'   mean temperature.
#' @param wrap Logical; if \code{TRUE}, allow wrap‑around rolling quarters
#'   (e.g., Dec–Jan–Feb). If \code{FALSE} (default), only non‑wrapping windows
#'   \code{[1..3], [2..4], ..., [10..12]} are considered.
#' @param filename Optional file path to write the output raster
#' ("" for in‑memory).
#'
#' @return A [terra::SpatRaster] with one layer representing **BIO8**
#' (Mean Temperature of Wettest Quarter, units °C).
#'
#' @references
#' Karger, D.N., et al. (2022). CHELSA V2.1: High‑resolution monthly and annual
#' climatologies for the Earth land surface areas. *Earth System Science Data*,
#' 14, 5573–5610.
#' Hijmans, R.J., et al. (2005). Very high resolution interpolated climate
#' surfaces for global land areas. *International Journal of Climatology*,
#' 25(15), 1965–1978.
#' See also CHELSA Bioclim dataset overview:
#' <https://www.chelsa-climate.org/datasets/chelsa_bioclim>
#' @export
#' @examples
#' # Example (with mock data generators)
#' pr_ex <- mock_pr() # 12-layer monthly precipitation (mm)
#' tas_ex <- mock_tas() # 12-layer monthly mean temperature (°C)
#'
#' # Compute BIO8 without wrap-around (only Jan–Dec contiguous quarters)
#' bio8 <- bio_08(pr_ex, tas_ex, wrap = FALSE)
#' bio8
#'#' bio_08 — Mean Temperature of Wettest Quarter (BIO8)
#'
#' Compute **BIO8 (Mean Temperature of Wettest Quarter)** from monthly
#' precipitation (`pr`) and monthly mean temperature (`tas`) rasters following
#' the CHELSA/WorldClim Bioclimatic Variables framework.
#'
#' @details
#' **BIO8** is the **average monthly mean temperature over the wettest
#' 3‑month period** of the year, computed per cell. This implementation:
#' \enumerate{
#'   \item Identifies, for each cell, the starting month of the **wettest quarter**
#'         by taking rolling 3‑month precipitation sums (optionally allowing
#'         wrap‑around windows, e.g., Dec–Jan–Feb when \code{wrap = TRUE}) and
#'         selecting the maximum-sum window via
#'         \code{parallel_which_max_quarter()}.
#'   \item Uses that quarter index to compute the **mean of the three monthly
#'         mean temperatures** from \code{tas} for the same months via
#'         \code{parallel_average_quarter()}.
#' }
#'
#' @section NA handling:
#' - If \code{na_rm = TRUE}, missing values (\code{NA}) within a 3‑month window
#'   are ignored when computing sums and means. A window with all \code{NA}s
#'   yields \code{NA}.
#' - If \code{na_rm = FALSE} (default), any \code{NA} in a window propagates,
#'   resulting in \code{NA} for that cell.
#'
#' @section Inputs and units:
#' - \code{pr}: monthly total precipitation (e.g., mm), 12 layers (Jan–Dec).
#' - \code{tas}: monthly mean temperature (°C), 12 layers (Jan–Dec).
#'
#' Both rasters must share identical spatial geometry (extent, resolution, CRS)
#' and the same number of rows/columns.
#'
#' @param pr A [terra::SpatRaster] with **12 layers** of monthly total
#'   precipitation (Jan–Dec).
#' @param tas A [terra::SpatRaster] with **12 layers** of monthly mean
#'   temperature (Jan–Dec).
#' @param wrap Logical; if \code{TRUE}, allow wrap‑around rolling quarters
#'   (e.g., Dec–Jan–Feb). If \code{FALSE} (default), only non‑wrapping windows
#'   \code{[1..3], [2..4], ..., [10..12]} are considered.
#' @param na_rm Logical; if \code{TRUE}, ignore \code{NA}s when selecting and
#'   averaging quarters. If \code{FALSE} (default), any \code{NA} in a window
#'   propagates to the result.
#' @param filename Optional file path for writing the output raster
#'   (use \code{""} for in‑memory).
#'
#' @return A [terra::SpatRaster] with one layer representing **BIO8**
#'   (Mean Temperature of Wettest Quarter, units °C).
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
#' pr_ex  <- mock_pr()   # precipitation (mm)
#' tas_ex <- mock_tas()  # mean temperature (°C)
#'
#' # Compute BIO8 without wrap-around (only Jan–Dec contiguous quarters)
#' bio8 <- bio_08(pr_ex, tas_ex, wrap = FALSE)
#' bio8
#'
#' # Allow wrap-around quarters (e.g., Dec–Jan–Feb)
#' bio8_wrap <- bio_08(pr_ex, tas_ex, wrap = TRUE)
#' bio8_wrap
#'
#' # Handle NA values by ignoring them
#' pr_na  <- make_1x1_12(c(100, NA, 200, 50, 50, 50, 50, 50, 50, 50, 50, 50))
#' tas_na <- make_1x1_12(c(NA, NA, 30, 1,1,1,1,1,1,1,1,1))
#' bio8_na <- bio_08(pr_na, tas_na, wrap = FALSE, na_rm = TRUE)
#' bio8_na
#'
#' @export
bio_08 <- function(pr, tas, wrap = FALSE, na_rm = FALSE, filename = "") {
  checkmate::assert_class(pr, "SpatRaster")
  checkmate::assert_class(tas, "SpatRaster")
  checkmate::assert_true(terra::nlyr(pr) == 12,
    .var.name = "pr must have 12 layers (monthly data)"
  )
  checkmate::assert_true(terra::nlyr(tas) == 12,
    .var.name = "tas must have 12 layers (monthly data)"
  )
  checkmate::assert_true(all(dim(pr)[1:2] == dim(tas)[1:2]),
    .var.name = "pr and tas must have same dimensions"
  )
  checkmate::assert_string(filename, null.ok = TRUE)

  # Create output raster (single layer)
  out <- terra::rast(pr, nlyr = 1)

  terra::readStart(pr)
  terra::readStart(tas)
  on.exit(terra::readStop(pr), add = TRUE)
  on.exit(terra::readStop(tas), add = TRUE)

  ncols <- terra::ncol(pr)

  b <- terra::writeStart(out, filename, overwrite = TRUE)
  on.exit(try(terra::writeStop(out), silent = TRUE), add = TRUE)
  for (i in 1:b$n) {
    v_pr <- terra::readValues(pr,
      row = b$row[i], nrows = b$nrows[i],
      col = 1, ncols = ncols, mat = TRUE
    )
    r_max_pr <- parallel_which_max_quarter(v_pr, wrap = wrap, na_rm = na_rm)

    v_tas <- terra::readValues(tas,
      row = b$row[i], nrows = b$nrows[i],
      col = 1, ncols = ncols, mat = TRUE
    )

    r <- parallel_average_quarter(idx = as.matrix(r_max_pr),
                                  mat =  v_tas,
                                  wrap = wrap, 
                                  na_rm = na_rm)

    terra::writeValues(out, r, b$row[i], b$nrows[i])
  }
  
  terra::writeStop(out)
  names(out) <- "bio_08"
  out
}
