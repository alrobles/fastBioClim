#' Compute row means in parallel
#'
#' @param mat A numeric matrix
#' @return A numeric vector containing row means
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' parallel_average(mat)
#' @export
parallel_average <- function(mat) {
  # --- Assertions using checkmate ---
  checkmate::assert_matrix(mat,
    mode = "numeric",
    any.missing = FALSE,
    min.rows = 1,
    min.cols = 1
  )

  # Call C++ implementation
  rcpp_parallel_average(mat)
}
