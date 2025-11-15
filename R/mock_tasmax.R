#' Create a mock CHELSA tasmax raster stack
#'
#' This function generates a synthetic \code{SpatRaster} stack that mimics the
#' structure of CHELSA monthly maximum temperature (tasmax) rasters. It is
#' intended for testing and unit validation of functions that process CHELSA
#' climate data.
#'
#' @details
#' - The stack contains 12 layers, one for each month (January–December).
#' - Each layer is a 20 × 20 grid of constant values.
#' - The value in each layer equals \code{month * 2}, i.e. January = 2,
#'   February = 4, ..., December = 24.
#' - The projection is WGS84 (\code{EPSG:4326}), consistent with CHELSA rasters.
#' - Layer names follow CHELSA convention:
#' \code{tasmax_01}, \code{tasmax_02}, ..., \code{tasmax_12}.
#'
#' @return A \code{SpatRaster} object with 12 layers representing mock monthly
#' tasmax data.
#'
#' @examples
#' # Create mock tasmax stack
#' mock <- mock_tasmax()
#'
#' @seealso \code{\link[terra]{rast}}, \code{\link[terra]{SpatRaster}}
#'
#' @export
mock_tasmax <- function(){
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
    terra::values(r) <- month * 2
    rasters[[month]] <- r
  }
  
  # Stack into a single SpatRaster
  tasmax_stack <- terra::rast(rasters)
  
  # Assign layer names (like CHELSA convention: tasmax_01 ... tasmax_12)
  names(tasmax_stack) <- sprintf("tasmax_%02d", 1:12)
  
  tasmax_stack
}

