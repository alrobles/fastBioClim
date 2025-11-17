#' Row-wise minimum (parallel)
#'
#' Fast row-wise **minimum value** using a parallel C++ implementation via
#' \pkg{RcppParallel}. Designed for large, tall matrices created from raster
#' stacks (rows = cells, columns = months), as commonly used with CHELSA
#' climatologies.
#'
#' @details
#' Typical CHELSA Bioclim use cases include computing:
#' - **BIO6** (Min Temperature of Coldest Month): the per-cell minimum across
#'   the 12 monthly temperature layers of the relevant CHELSA temperature
#'   product.
#' - Analogous operations for other variables where the month-wise minimum is
#'   an intermediate step.
#'
#' References:
#' - CHELSA Bioclim dataset:
#'   <https://www.chelsa-climate.org/datasets/chelsa_bioclim>
#' - Karger et al. (2022). CHELSA V2.1: High-resolution monthly and annual
#'   climatologies for the Earth land surface areas.
#'   *Earth System Science Data*, 14, 5573–5610.
#'
#' @param mat A **numeric matrix** with rows representing spatial units (e.g.,
#'   grid cells) and columns representing measurements (e.g., the 12 months).
#'
#' @return A **numeric vector** of length `nrow(mat)` with the row-wise minimum
#'   values. Row names are preserved (if present).
#'
#' @section NA handling:
#' If any `NA`/`NaN` is present in a row, the result for that row is `NA`
#' (equivalent to `na.rm = FALSE`). Rows with zero columns are invalid and
#' should be filtered upstream.
#'
#' @examples
# Simple example: rows are cells, columns are months
#' mat <- matrix(1:9,
#'   ncol = 3,
#'   dimnames = list(paste0("cell", 1:3), paste0("m", 1:3))
#' )
# Rows are: c(1,4,7), c(2,5,8), c(3,6,9) -> minima: 1, 2, 3
#' mn <- parallel_row_min(mat)
#' mn
#'
#' # CHELSA-style usage:
#' tas <- mock_tas()
#' # Suppose 'tas' values were read blockwise to a matrix 'v'
#' # (cells x 12 months), you could get the warmest month value per cell as:
#' tas_matrix <- terra::as.matrix(tas)
#' coldest <- parallel_row_max(tas_matrix)
#' all(coldest == 12)
#' @seealso [parallel_row_max()], [parallel_sd()], [parallel_variance()]
#' @export
parallel_row_min <- function(mat) {
  # --- Assertions using checkmate ---
  checkmate::assert_matrix(
    mat,
    mode = "numeric",
    any.missing = TRUE, # allow NA; C++ returns NA for rows with NA
    min.rows = 1,
    min.cols = 1
  )

  # Call C++ implementation (returns nrow x 1 matrix with values)
  res_mat <- rcpp_parallel_which_min_row(mat)

  # Return vector, preserving row names
  out <- as.numeric(res_mat[, 1])
  names(out) <- rownames(mat)
  out
}
