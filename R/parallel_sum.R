#' Parallel sum per row
#'
#' Computes the **sum of all columns per row** in a numeric matrix using a
#' parallelized C++ routine for efficiency.
#'
#' @details
#' This function calculates the **row-wise sum** of a numeric matrix:
#' \enumerate{
#'   \item Each row is processed in parallel for speed.
#'   \item If \code{na_rm = FALSE} (default), any \code{NA} in a row propagates,
#'         resulting in \code{NA} for that row.
#'   \item If \code{na_rm = TRUE}, missing values (\code{NA}) are ignored when
#'         summing. If all values in a row are \code{NA}, the result is
#'         \code{NA}.
#' }
#'
#' @section Inputs:
#' - \code{mat}: A numeric matrix with one or more rows and columns.
#'
#' @param mat Numeric matrix of values (rows = observations,
#' columns = variables).
#'   Must have at least one row and one column.
#' @param na_rm Logical; if \code{TRUE}, ignore \code{NA}s when summing.
#'   If \code{FALSE} (default), any \code{NA} propagates to the result.
#'
#' @return A numeric vector of length \code{nrow(mat)} containing the sum of
#'   each row. Row names are preserved if present.
#'
#' @examples
#' # Basic usage
#' mat <- matrix(1:9, nrow = 3)
#' parallel_sum(mat)
#' # [1]  6 15 24
#'
#' # Handle NA values
#' mat_na <- matrix(c(1, NA, 3, 4), nrow = 2)
#' parallel_sum(mat_na, na_rm = TRUE)
#' # [1]  4  4
#'
#' @seealso [rcpp_parallel_sum()]
#'
#' @export
parallel_sum <- function(mat, na_rm = FALSE) {
  checkmate::assert_matrix(mat,
    mode = "numeric",
    any.missing = TRUE,
    min.cols = 1,
    min.rows = 1
  )

  res <- rcpp_parallel_sum(mat, na_rm = na_rm)
  out <- as.numeric(res[, 1])
  names(out) <- rownames(mat)
  out
}
