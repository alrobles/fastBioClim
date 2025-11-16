#' Compute row means in parallel
#'
#' @param mat A numeric matrix
#' @return A numeric matrix with one column containing row means
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' parallel_average(mat)
#' @export
parallel_average <- function(mat) {
  if (!is.matrix(mat)) stop("Input must be a numeric matrix")
  rcpp_parallel_average(mat)
}
