#' Calculate bio_12 bioclimatic variable from Chelsa data
#'
#' @param year The target year to calculate the bioclim 
#' @param targetDir The directory where chelsa variables are stored
#' @param outputDir The root output directory where to store the bioclimatic variables for that year
#'
#' @return A raster tif file with the bioclimatic variable 1
#' @export
#'
#' @examples
chelsa_bio_12 <- function(year, targetDir = "/media/reumanlab/backup/chelsav2_USA/", outputDir = "/media/reumanlab/backup/test_output/") {
  targetDir <- paste0(targetDir, year, "/")
  outputDir <- paste0(outputDir, year, "/")
  
  if(!dir.exists(outputDir)){
    dir.create(outputDir)
  }
  varString <- "pr_"
  varFiles <- list.files(path = targetDir, pattern = varString, full.names = TRUE)
  pr <- terra::rast(varFiles)
  #check units. We divide over 100 to be consistent with 
  # magnitude 
  pr <- pr/100
  filename <- paste0(outputDir, "bio_12.tif")
  fastBioClim::bio_12(pr, filename = filename)
}
