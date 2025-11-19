#' Parallel sum per row
#'
#' Computes the **sum of all columns per row** in a numeric matrix using
#' a parallelized C++ routine.
#'
#' @param mat Numeric matrix of values (rows = observations, columns = variables).
#' @return A numeric vector of length `nrow(mat)` with the per-row sums.
#' @export
parallel_sum <- function(mat) {
  checkmate::assert_matrix(mat,
    mode = "numeric",
    any.missing = TRUE,
    min.cols = 1,
    min.rows = 1
  )

  res <- rcpp_parallel_sum(mat)
  out <- as.numeric(res[, 1])
  names(out) <- rownames(mat)
  out
}
