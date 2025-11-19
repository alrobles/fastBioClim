#' Create a mock CHELSA tasmin raster stack
#'
#' This function generates a synthetic \code{SpatRaster} stack that mimics the
#' structure of CHELSA monthly maximum temperature (tasmin) rasters. It is
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
#' \code{tasmin_01}, \code{tasmin_02}, ..., \code{tasmin_12}.
#'
#' @return A \code{SpatRaster} object with 12 layers representing mock monthly
#' tasmin data.
#'
#' @examples
#' # Create mock tasmin stack
#' mock <- mock_tasmin()
#'
#' @seealso \code{\link[terra]{rast}}, \code{\link[terra]{SpatRaster}}
#'
#' @export
mock_tasmin <- function(){
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
    terra::values(r) <- 0
    rasters[[month]] <- r
  }
  
  # Stack into a single SpatRaster
  tasmin_stack <- terra::rast(rasters)
  
  # Assign layer names (like CHELSA convention: tasmin_01 ... tasmin_12)
  names(tasmin_stack) <- sprintf("tasmin_%02d", 1:12)
  
  tasmin_stack
}

