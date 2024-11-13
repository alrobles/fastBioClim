#' Calculate bio_1 bioclimatic variable from Chelsa data
#'
#' @param year The target year to calculate the bioclim 
#' @param targetDir The directory where chelsa variables are stored
#' @param outputDir The root output directory where to store the bioclimatic variables for that year
#'
#' @return A raster tif file with the bioclimatic variable 1
#' @export
#'
#' @examples
chelsa_bio_1 <- function(year, targetDir = "/media/reumanlab/backup/chelsav2_USA/", outputDir = "/media/reumanlab/backup/test_output/") {
  targetDir <- paste0(targetDir, year, "/")
  outputDir <- paste0(outputDir, year, "/")
  
  if(!dir.exists(outputDir)){
    dir.create(outputDir)
  }
  varString <- "tas_"
  varFiles <- list.files(path = targetDir, pattern = varString, full.names = TRUE)
  tas <- terra::rast(varFiles)
  filename <- paste0(outputDir, "bio_1.tif")
  fastBioClim::bio_1(tas, filename = filename)
}
