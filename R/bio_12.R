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
#'   \item Computes the **sum of all monthly precipitation values** for each
#'   cell.
#' }
#'
#' The sum is computed using \code{parallel_sum(mat, wrap = ..., na_rm = ...)}.
#'
#' @section NA handling:
#' - If \code{na_rm = TRUE}, missing values (\code{NA}) are ignored when
#' summing.
#' - If all 12 months are \code{NA}, the result is \code{NA}.
#' - If \code{na_rm = FALSE} (default), any \code{NA} in the 12 months
#' propagates, resulting in \code{NA} for that cell.
#'
#' @section Inputs and units:
#' - \code{pr}: monthly precipitation (mm), 12 layers (Jan–Dec).
#'
#' The raster must be a \code{terra::SpatRaster} with 12 layers. When writing to
#' disk, the output inherits geometry (extent, resolution, CRS) from \code{pr}.
#'
#' @param pr A [terra::SpatRaster] with **12 layers** of monthly precipitation
#' (Jan–Dec).
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
#' @export
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
#'   \item Computes the **sum of all monthly precipitation values** for each cell
#'   using a parallelized C++ implementation for optimal performance.
#' }
#'
#' @section NA handling:
#' - If \code{na_rm = TRUE}, missing values (\code{NA}) are ignored when summing.
#'   Cells with all-NA months remain \code{NA}.
#' - If \code{na_rm = FALSE} (default), any \code{NA} in the 12 months propagates
#'   to the result.
#'
#' @section Inputs and units:
#' - \code{pr}: monthly precipitation (mm), 12 layers (Jan–Dec).
#' - Output: Annual precipitation in mm.
#'
#' @param pr A [terra::SpatRaster] with \strong{12 layers} of monthly precipitation.
#' @param filename Optional output file path (default \code{""} = in-memory).
#' @param na_rm Logical; if \code{TRUE}, ignore \code{NA}s when summing.
#'
#' @return A [terra::SpatRaster] with one layer representing **BIO12** (Annual Precipitation).
#'
#' @references
#' Karger, D.N., et al. (2022). CHELSA V2.1.
#' \emph{Earth System Science Data}, 14, 5573–5610.
#' 
#' Hijmans, R.J., et al. (2005). Very high resolution interpolated climate surfaces.
#' \emph{International Journal of Climatology}, 25, 1965–1978.
#' 
#' @examples
#' \dontrun{
#' library(terra)
#' # Create sample monthly precipitation data (12 layers)
#' r <- rast(nrows=100, ncols=100, nlyrs=12, vals=runif(12*100*100, 0, 100))
#' names(r) <- month.abb
#' # Compute annual precipitation
#' bio12 <- bio_12(r)
#' plot(bio12)
#' }
#' @export
bio_12 <- function(pr, filename = "", na_rm = FALSE) {
  # Input validation
  checkmate::assert_class(pr, "SpatRaster")
  checkmate::assert_true(
    terra::nlyr(pr) == 12,
    .var.name = "pr must have exactly 12 layers (monthly data)"
  )
  checkmate::assert_string(filename)
  checkmate::assert_flag(na_rm)
  
  # Create output template
  out <- terra::rast(pr, nlyr = 1)
  names(out) <- "bio_12"
  
  # Set up reading
  terra::readStart(pr)
  on.exit(terra::readStop(pr), add = TRUE)
  
  # Set up writing
  write_info <- terra::writeStart(out, filename, overwrite = TRUE)
  on.exit(try(terra::writeStop(out), silent = TRUE), add = TRUE)
  
  # Process in blocks
  for (i in seq_len(write_info$n)) {
    # Read block
    v_pr <- terra::readValues(
      pr,
      row = write_info$row[i],
      nrows = write_info$nrows[i],
      col = 1,
      ncols = terra::ncol(pr),
      mat = TRUE
    )
    
    # Compute annual precipitation
    r <- rcpp_parallel_sum(v_pr, na_rm = na_rm)
    
    # Write block
    terra::writeValues(out, r, write_info$row[i], write_info$nrows[i])
  }
  
  # Finalize writing
  terra::writeStop(out)
  return(out)
}