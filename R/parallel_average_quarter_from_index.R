#' Average of 3-consecutive columns (quarter) given per-row starting indices
#'
#' Computes, for each row, the average of a 3-consecutive-column window
#' (a "quarter") starting at the provided index. Uses a fast C++ routine
#' \code{rcpp_average_get_quarter()} that sums with \code{NA}-skipping and
#' divides by the count of non-\code{NA} values among the 3 elements.
#'
#' @details
#' - The input \code{idx} is a **1-based** starting index per row (typical in R).
#' - If \code{wrap = FALSE} (default), valid indices are in
#'   \code{1..(ncol(mat) - 2)} and windows are \code{[j, j+1, j+2]} entirely
#'   within the matrix.
#' - If \code{wrap = TRUE}, the wrapper virtually extends \code{mat} by
#'   \code{cbind(mat, mat[,1:2])}, so starts in \code{1..ncol(mat)} are valid
#'   and windows can wrap (e.g., Dec–Jan–Feb).
#'
#' @section NA handling:
#' The underlying C++ sums with an \emph{NA-skipping} reducer and divides by the
#' number of non-\code{NA} elements among the three; if all three are \code{NA},
#' the C++ divides by zero and returns a non-finite value. This wrapper converts
#' any non-finite result to \code{NA_real_}.
#'
#' @param idx Integer/numeric vector (length \code{nrow(mat)}), 1-based starting
#'   column index per row; \code{NA} allowed.
#' @param mat Numeric matrix (rows = cells, columns = months).
#' @param wrap Logical; if \code{TRUE}, allow wrap-around windows by internally
#'   extending \code{mat} with its first two columns.
#'
#' @return A numeric vector of length \code{nrow(mat)} with the average of the
#'   3-month window per row. Row names are preserved from \code{mat} if present.
#'
#' @examples
#' # Example: each row is 1:12, 13:24, 25:36
#' m1 <- matrix(1:(12*3), nrow = 3, byrow = TRUE,
#'              dimnames = list(paste0("cell", 1:3), paste0("m", 1:12)))
#' # Start at month 10 for all rows (window: 10,11,12) -> average of (10,11,12), (22,23,24), (34,35,36)
#' idx <- rep(10L, nrow(m1))
#' avg <- parallel_average_quarter_from_index(idx, m1, wrap = FALSE)
#' avg
#'
#' # With wrap-around: start at 12 (window: 12, 1, 2)
#' idx2 <- c(12L, 12L, 12L)
#' avg_wrap <- parallel_average_quarter_from_index(idx2, m1, wrap = TRUE)
#' avg_wrap
#'
#' @export
parallel_average_quarter_from_index <- function(idx, mat, wrap = FALSE) {
  # --- Assertions ---
  checkmate::assert_matrix(mat, mode = "numeric", min.rows = 1, min.cols = 3, any.missing = TRUE)
  checkmate::assert_flag(wrap)
  checkmate::assert_numeric(idx, any.missing = TRUE, len = nrow(mat))
  
  # Coerce idx to integer (1-based)
  idx <- as.integer(idx)
  
  # Build the matrix we pass to C++.
  # If wrap = TRUE: allow j in 1..ncol(mat) by extending columns 1:2 at the end.
  # If wrap = FALSE: enforce j in 1..(ncol(mat)-2).
  if (wrap) {
    if (ncol(mat) < 3) stop("mat must have at least 3 columns")
    mat_use <- cbind(mat, mat[, 1:2, drop = FALSE])
    max_start <- ncol(mat)  # valid starts: 1..ncol(mat)
  } else {
    mat_use <- mat
    max_start <- ncol(mat) - 2L  # valid starts: 1..(ncol(mat)-2)
  }
  
  # Bounds check (ignore NA starts)
  if (max_start < 1L) {
    stop("Not enough columns in 'mat' for a 3-column window (need at least 3).")
  }
  bad <- !is.na(idx) & (idx < 1L | idx > max_start)
  if (any(bad)) {
    stop(sprintf(
      "Found %d invalid start indices; valid range is [%d, %d] (wrap = %s).",
      sum(bad), 1L, max_start, as.character(wrap)
    ))
  }
  
  # Prepare 0-based index matrix for C++ (keep NA as NA)
  ix0 <- idx
  ix0[!is.na(ix0)] <- ix0[!is.na(ix0)] - 1L
  ixQuarter <- matrix(ix0, ncol = 1)
  
  # Call C++: returns nrow x 1 numeric matrix of averages
  res_mat <- rcpp_average_get_quarter(ixQuarter, mat_use)
  
  # Convert to vector and sanitize non-finite to NA
  out <- as.numeric(res_mat[, 1])
  out[!is.finite(out)] <- NA_real_
  
  # Preserve row names
  names(out) <- rownames(mat)
  out
}