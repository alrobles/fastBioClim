#' Rolling quarter (3-month) minimum index per row (parallel)
#'
#' Returns, for each row, the **starting column index** (1-based) of the
#' 3-consecutive-column window whose **sum is minimum**, using the parallel
#' C++ routine \code{rcpp_parallel_which_min_rolling_quarter()}.
#'
#' @inheritParams parallel_which_max_rolling_quarter
#'
#' @return An integer vector of length `nrow(mat)` with **1-based** starting
#'   indices. Row names are preserved from `mat` if present.
#'
#' @export
parallel_which_min_rolling_quarter <- function(mat, wrap = FALSE, na_rm = FALSE) {
  checkmate::assert_matrix(mat, mode = "numeric", any.missing = TRUE, min.rows = 1, min.cols = 3)
  checkmate::assert_flag(wrap)
  checkmate::assert_flag(na_rm)
  
  idx_mat <- rcpp_parallel_which_min_rolling_quarter(mat, wrap = wrap, na_rm = na_rm)
  out <- as.integer(idx_mat[, 1])
  names(out) <- rownames(mat)
  out
}