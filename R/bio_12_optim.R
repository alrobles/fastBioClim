#' Optimized BIO12 computation using RcppParallel
#' @param pr SpatRaster with 12 layers (monthly precipitation)
#' @param filename Optional output file path
#' @param na_rm Logical, remove NA values
#' @return SpatRaster with one layer (annual precipitation)
#' @export
bio_12_optim <- function(pr, filename = "", na_rm = FALSE,
                           nthreads = parallel::detectCores(),
                           block_factor = 4) {
  checkmate::assert_class(pr, "SpatRaster")
  checkmate::assert_true(terra::nlyr(pr) == 12)
  checkmate::assert_string(filename, null.ok = TRUE)
  
  out <- terra::rast(pr, nlyr = 1)
  names(out) <- "bio_12"
  
  terra::readStart(pr)
  on.exit(terra::readStop(pr), add = TRUE)
  
  write_info <- terra::writeStart(out, filename, overwrite = TRUE,
                                  n = block_factor * terra::nlyr(pr))
  on.exit(try(terra::writeStop(out), silent = TRUE), add = TRUE)
  
  for (i in seq_len(write_info$n)) {
    v_pr <- terra::readValues(pr, row = write_info$row[i],
                              nrows = write_info$nrows[i],
                              col = 1, ncols = terra::ncol(pr), mat = TRUE)
    r <- parallel_sum(v_pr, na_rm = na_rm, nthreads = nthreads)
    terra::writeValues(out, r, write_info$row[i], write_info$nrows[i])
  }
  
  terra::writeStop(out)
  out
}