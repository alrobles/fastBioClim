#' Row-wise sample variance (parallel)
#'
#' Fast row-wise **sample variance** using a parallel C++ implementation
#' (Welford one-pass update via \pkg{RcppParallel}). This is especially useful
#' for large rasters represented as matrices of size \eqn{n_{\text{cells}}
#' \times 12} (months), e.g., when computing bioclimatic variables from
#' CHELSA/WorldClim monthly climatologies.
#'
#' @details
#' Typical uses include intermediate steps in **CHELSA Bioclim** calculations,
#' such as computing monthly temperature/precipitation dispersion metrics. For
#' example, CHELSA **BIO4 (Temperature Seasonality)** uses the standard
#' deviation of monthly mean temperature scaled by 100; this function provides
#' the row-wise variance part of that workflow (see also [`parallel_sd()`]).
#'
#' References:
#' - CHELSA Bioclim dataset:
#' <https://www.chelsa-climate.org/datasets/chelsa_bioclim>
#' - Karger et al. (2022). CHELSA V2.1: High-resolution monthly and annual
#'   climatologies for the Earth land surface areas. *Earth Syst. Sci. Data*,
#'   14, 5573–5610.
#'
#' @param mat A **numeric matrix** (rows = units such as grid cells;
#' columns = measurements, e.g., 12 monthly values).
#'
#' @return A **numeric vector** of length `nrow(mat)` with the
#' **row-wise sample variance** (denominator \eqn{n-1}). Row names are preserved
#' (if present).
#'
#' @section NA handling:
#' If any `NA`/`NaN` is present in a row, the result for that row is `NA`
#' (mirrors base R with `na.rm = FALSE`). Rows with fewer than 2 columns also
#' yield `NA`.
#'
#' @examples
#' set.seed(1)
#' mat <- matrix(rnorm(5 * 12, 15, 6),
#'   nrow = 5, ncol = 12,
#'   dimnames = list(paste0("cell", 1:5), month.abb)
#' )
#' v_par <- parallel_variance(mat)
#' v_ref <- apply(mat, 1, var)
#' all.equal(v_par, v_ref)
#'
#' # Example of use in a CHELSA-style workflow (variance part)
#' # (BIO4 uses SD * 100; see parallel_sd())
#' @seealso [parallel_sd()]
#' @export
parallel_variance <- function(mat) {
  # --- Assertions using checkmate ---
  checkmate::assert_matrix(
    mat,
    mode = "numeric",
    any.missing = TRUE, # allow NA; C++ returns NA for rows with NA
    min.rows = 1,
    min.cols = 1
  )

  # Call C++ implementation (returns nrow x 1 matrix)
  res_mat <- rcpp_parallel_variance(mat)

  # Return vector, preserving row names
  out <- as.numeric(res_mat[, 1])
  names(out) <- rownames(mat)
  out
}
