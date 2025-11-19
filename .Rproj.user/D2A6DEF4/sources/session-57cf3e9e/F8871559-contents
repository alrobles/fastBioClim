#' Row-wise sample standard deviation (parallel)
#'
#' Fast row-wise **sample standard deviation** using a parallel C++
#' implementation (Welford one-pass update via \pkg{RcppParallel}). Designed for
#' large matrices derived from rasters (e.g., rows are cells and columns are the
#' 12 months), and directly applicable to CHELSA Bioclimatic variable
#' calculations.
#'
#' @details
#' This is particularly useful for **CHELSA BIO4 (Temperature Seasonality)**,
#' which is defined as the **standard deviation of monthly mean temperature
#' multiplied by 100**: \deqn{\mathrm{BIO4} = 100
#' \times \mathrm{sd}(\text{tas}_{\mathrm{Jan..Dec}}).}
#' You can compute the SD per cell via `parallel_sd()` and scale by 100.
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
#' **row-wise sample SD** (denominator \eqn{n-1}). Row names are preserved
#' (if present).
#'
#' @section NA handling:
#' If any `NA`/`NaN` is present in a row, the result for that row is `NA`
#' (mirrors base R with `na.rm = FALSE`). Rows with fewer than 2 columns also
#' yield `NA`.
#' @examples
#' set.seed(1)
#' mat <- matrix(rnorm(5 * 12, 15, 6),
#'   nrow = 5, ncol = 12,
#'   dimnames = list(paste0("cell", 1:5), month.abb)
#' )
#' s_par <- parallel_sd(mat)
#' s_ref <- apply(mat, 1, sd)
#' all.equal(s_par, s_ref)
#'
#' # BIO4 (Temperature Seasonality) from monthly mean temperature matrix:
#' # bio4_vec <- parallel_sd(mat) * 100
#'
#' @seealso [parallel_variance()]
#' @export
parallel_sd <- function(mat) {
  # --- Assertions using checkmate ---
  checkmate::assert_matrix(
    mat,
    mode = "numeric",
    any.missing = TRUE, # allow NA; C++ returns NA for rows with NA
    min.rows = 1,
    min.cols = 1
  )

  # Call C++ implementation (returns nrow x 1 matrix)
  res_mat <- rcpp_parallel_sd(mat)

  # Return vector, preserving row names
  out <- as.numeric(res_mat[, 1])
  names(out) <- rownames(mat)
  out
}
