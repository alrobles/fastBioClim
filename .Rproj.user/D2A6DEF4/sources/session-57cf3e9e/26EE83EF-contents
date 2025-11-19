#' Row-wise index of maximum three-matrix sum (parallel)
#'
#' Returns, for each row, the **column index** (1-based) at which the sum
#' \code{mat_1[,j] + mat_2[,j] + mat_3[,j]} is **maximum**, using a fast
#' parallel C++ implementation via \pkg{RcppParallel}.
#'
#' This is useful when the maximum is computed from three aligned monthly
#' matrices (e.g., components that form a composite score per month).
#'
#' @details
#' - If `na_rm = FALSE` (default), any `NA`/`NaN` present **anywhere in the row**
#'   of any input matrix produces `NA` for that row (strict policy).
#' - If `na_rm = TRUE`, positions where any of the three matrices has `NA`
#'   are **skipped**; if **all** positions are skipped (i.e., every `j` has
#'   at least one `NA`), the row result is `NA`.
#'
#' All three matrices must have the **same dimensions** and be numeric.
#'
#' @param mat_1,mat_2,mat_3 Numeric matrices with the same dimensions
#'   (rows = spatial units/cells, columns = months or categories).
#' @param na_rm Logical; if `TRUE`, skip positions with `NA` when searching
#'   for the maximum. If `FALSE` (default), any `NA` in the row yields `NA`.
#'
#' @return An **integer vector** of length `nrow(mat_1)` with the **1-based**
#'   column index of the maximum three-way sum per row. Row names are preserved
#'   from `mat_1` if present.
#'
#' @section Example:
#' \preformatted{
#' # Three simple 3x4 matrices
#' set.seed(1)
#' m1 <- matrix(1:12, nrow = 3, byrow = TRUE)
#' m2 <- matrix(rep(c(0, 5, -2, 1), each = 3), nrow = 3, byrow = TRUE)
#' m3 <- matrix(3, nrow = 3, ncol = 4)
#'
#' # Per row, we take which j maximizes m1[,j] + m2[,j] + m3[,j]
#' idx <- parallel_which_max_quarter_idx(m1, m2, m3)
#' idx  # integer vector (1-based indices)
#' }
#'
#' @seealso [parallel_which_min_quarter_idx()] for the minimum index counterpart.
#' @export
parallel_which_max_quarter_idx <- function(mat_1, mat_2, mat_3, na_rm = FALSE) {
  # --- Assertions using checkmate ---
  checkmate::assert_matrix(mat_1, mode = "numeric", any.missing = TRUE, min.rows = 1, min.cols = 1)
  checkmate::assert_matrix(mat_2, mode = "numeric", any.missing = TRUE, nrows = nrow(mat_1), ncols = ncol(mat_1))
  checkmate::assert_matrix(mat_3, mode = "numeric", any.missing = TRUE, nrows = nrow(mat_1), ncols = ncol(mat_1))
  checkmate::assert_flag(na_rm)
  
  # Call C++ (returns nrow x 1 IntegerMatrix of indices)
  res_mat <- rcpp_parallel_which_max_quarter_idx(mat_1, mat_2, mat_3, na_rm = na_rm)
  
  # Convert to integer vector, preserve row names from mat_1
  out <- as.integer(res_mat[, 1])
  names(out) <- rownames(mat_1)
  out
}