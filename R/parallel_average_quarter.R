#' Parallel average over quarter (3 months) per row
#'
#' Computes the average of the 3-consecutive-month window per row, starting at
#' indices provided in `idx` (1-based). NAs are ignored in the average;
#' if the three values are all NA, the result is NA. Optionally allows wrap-around.
#'
#' @param idx Numeric matrix with one column and `nrow(mat)` rows,
#'   containing **1-based** starting indices per row.
#' @param mat Numeric matrix of values (rows = cells, columns = months).
#' @param wrap Logical; if `TRUE`, allow wrap-around windows (e.g., Dec–Jan–Feb).
#'   Default `FALSE`.
#'
#' @return A numeric vector of length `nrow(mat)` with the averages. Row names
#'   from `mat` are preserved.
#' @export
parallel_average_quarter <- function(idx, mat, wrap = FALSE) {
  checkmate::assert_matrix(idx, mode = "numeric", ncols = 1, any.missing = TRUE)
  checkmate::assert_matrix(mat, mode = "numeric", any.missing = TRUE, min.cols = 3, min.rows = 1)
  checkmate::assert_flag(wrap)
  checkmate::assert_true(nrow(idx) == nrow(mat),
                         .var.name = "idx has same number of rows as mat")
  
  res <- rcpp_parallel_average_quarter(idx, mat, wrap = wrap)
  out <- as.numeric(res[, 1])
  names(out) <- rownames(mat)
  out
}