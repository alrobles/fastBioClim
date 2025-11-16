#' Create a mock CHELSA tas raster stack
#'
#' This function generates a synthetic \code{SpatRaster} stack that mimics the
#' structure of CHELSA monthly maximum temperature (tasmax) rasters. It is
#' intended for testing and unit validation of functions that process CHELSA
#' climate data.
#'
#' @details
#' - The stack contains 12 layers, one for each month (January–December).
#' - Each layer is a 20 × 20 grid of constant values.
#' - The value in each layer equals \code{month}, i.e. January = 1,
#'   February = 2, ..., December = 12.
#' - The projection is WGS84 (\code{EPSG:4326}), consistent with CHELSA rasters.
#' - Layer names follow CHELSA convention:
#' \code{tasmax_01}, \code{tasmax_02}, ..., \code{tasmax_12}.
#'
#' @return A \code{SpatRaster} object with 12 layers representing mock monthly
#' tasmax data.
#'
#' @examples
#' # Create mock tas stack
#' mock <- mock_tas()
#'
#' @seealso \code{\link[terra]{rast}}, \code{\link[terra]{SpatRaster}}
#' @export
mock_tas <- function(){
  # Define dimensions
  nrows <- 20
  ncols <- 20

  # Create empty SpatRaster template using WGS84
  r_template <- terra::rast(nrows = nrows, ncols = ncols,
                            xmin = -180, xmax = 180,
                            ymin = -90, ymax = 90,
                            crs = "EPSG:4326")

  # Generate 12 monthly rasters with values = month * 2
  rasters <- list()
  for (month in 1:12) {
    r <- r_template
    terra::values(r) <- month
    rasters[[month]] <- r
  }

  # Stack into a single SpatRaster
  tas_stack <- terra::rast(rasters)

  # Assign layer names (like CHELSA convention: tas_01 ... tas_12)
  names(tas_stack) <- sprintf("tas_%02d", 1:12)

  tas_stack
}

