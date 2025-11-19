#' bio_12 — Annual Precipitation (BIO12)
#'
#' Compute **BIO12 (Annual Precipitation)** from monthly precipitation (`pr`)
#' rasters following the CHELSA/WorldClim Bioclimatic Variables framework.
#'
#' @details
#' **BIO12** is the **total precipitation over all 12 months** of the year,
#' computed per cell. This implementation:
#' \enumerate{
#'   \item Reads monthly precipitation values from \code{pr} (12 layers).
#'   \item Computes the **sum of all monthly precipitation values** for each cell.
#' }
#'
#' The sum is computed using \code{parallel_sum(mat, wrap = ..., na_rm = ...)}.
#'
#' @section NA handling:
#' - If \code{na_rm = TRUE}, missing values (\code{NA}) are ignored when summing.
#'   If all 12 months are \code{NA}, the result is \code{NA}.
#' - If \code{na_rm = FALSE} (default), any \code{NA} in the 12 months propagates,
#'   resulting in \code{NA} for that cell.
#'
#' @section Inputs and units:
#' - \code{pr}: monthly precipitation (mm), 12 layers (Jan–Dec).
#'
#' The raster must be a \code{terra::SpatRaster} with 12 layers. When writing to
#' disk, the output inherits geometry (extent, resolution, CRS) from \code{pr}.
#'
#' @param pr A [terra::SpatRaster] with **12 layers** of monthly precipitation (Jan–Dec).
#' @param filename Optional file path for writing the output raster
#'   (use \code{""} for in‑memory).
#' @param na_rm Logical; if \code{TRUE}, ignore \code{NA}s when summing.
#'   If \code{FALSE} (default), any \code{NA} propagates to the result.
#'
#' @return A [terra::SpatRaster] with one layer representing **BIO12**
#'   (Annual Precipitation, units mm).
#'
#' @references
#' Karger, D.N., et al. (2022). CHELSA V2.1: High‑resolution monthly and annual
#' climatologies for the Earth land surface areas.
#' \emph{Earth System Science Data}, 14, 5573–5610. \cr
#' Hijmans, R.J., et al. (2005). Very high resolution interpolated climate
bio_12 <- function(pr, filename = "", na_rm = FALSE) {
  checkmate::assert_class(pr, "SpatRaster")
  checkmate::assert_true(terra::nlyr(pr) == 12,
    .var.name = "pr must have 12 layers (monthly data)"
  )
  checkmate::assert_string(filename, null.ok = TRUE)

  # Create output raster (single layer)
  out <- terra::rast(pr, nlyr = 1)

  terra::readStart(pr)
  on.exit(terra::readStop(pr), add = TRUE)

  ncols <- terra::ncol(pr)

  b <- terra::writeStart(out, filename, overwrite = TRUE)
  on.exit(try(terra::writeStop(out), silent = TRUE), add = TRUE)
  for (i in 1:b$n) {
    v_pr <- terra::readValues(pr,
      row = b$row[i], nrows = b$nrows[i],
      col = 1, ncols = ncols, mat = TRUE
    )
    r <- parallel_sum(v_pr, wrap = wrap, na_rm = na_rm)
    terra::writeValues(out, r, b$row[i], b$nrows[i])
  }

  terra::writeStop(out)
  names(out) <- "bio_12"
  out
}
