#' Rolling quarter (3-month) maximum index per row (parallel)
#'
#' Returns, for each row, the **starting column index** (1-based) of the
#' 3-consecutive-month window whose **score is maximum**.
#' The score is a 3-month **mean**:
#' - If `na_rm = FALSE` (default), any `NA` in a window **invalidates** that window.
#' - If `na_rm = TRUE`, the score is computed as the **mean over non-NA values**;
#'   windows with **all NA** are invalid. If **no valid windows** exist for a row,
#'   the result is `NA`.
#'
#' @param mat Numeric matrix (rows = cells, columns = months). Must have
#'   at least 3 columns.
#' @param wrap Logical; if `TRUE`, allow wrap-around windows (e.g., Dec–Jan–Feb).
#'   If `FALSE`, only windows `[1..3], [2..4], ..., [10..12]` are considered.
#' @param na_rm Logical; see details above.
#'
#' @return An integer vector of length `nrow(mat)` with **1-based** starting
#'   indices. Row names are preserved from `mat` if present.
#' @export
parallel_which_max_quarter <- function(mat, wrap = FALSE, na_rm = FALSE) {
  checkmate::assert_matrix(mat,
                           mode = "numeric",
                           any.missing = TRUE,
                           min.rows = 1,
                           min.cols = 3
  )
  checkmate::assert_flag(wrap)
  checkmate::assert_flag(na_rm)
  
  idx_mat <- rcpp_parallel_which_max_rolling_quarter(
    mat, wrap = wrap, na_rm = na_rm
  )
  out <- as.integer(idx_mat[, 1])
  names(out) <- rownames(mat)
  out
}
