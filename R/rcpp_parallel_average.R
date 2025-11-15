#' Parallel row means of a numeric matrix
#'
#' This function computes the mean of each row in a numeric matrix using
#' \pkg{RcppParallel}. It is designed to demonstrate parallel processing
#' with Rcpp and can be used for efficient row-wise averaging in large datasets.
#'
#' @param mat_1 A numeric matrix. Each row will be averaged.
#'
#' @return A numeric matrix with one column, where each entry is the mean
#'   of the corresponding row in \code{mat_1}.
#'
#' @examples
#' mat <- matrix(1:12, nrow = 3, byrow = TRUE)
#' rcpp_parallel_average(mat)
#' #      [,1]
#' # [1,]    2
#' # [2,]    6
#' # [3,]   10
#'
#' @seealso \code{\link[RcppParallel]{parallelFor}}, \code{\link[Rcpp]{NumericMatrix}}
#'
#' @export
#' @name rcpp_parallel_average
NULL