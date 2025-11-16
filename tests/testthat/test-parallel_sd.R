library(testthat)
library(checkmate)

test_that("parallel_sd computes row-wise sample SD correctly", {
  set.seed(123)
  mat <- matrix(rnorm(5 * 12, mean = 10, sd = 3),
    nrow = 5, ncol = 12,
    dimnames = list(paste0("cell", 1:5), month.abb)
  )

  result <- parallel_sd(mat)

  # Expected result from base R
  expected <- apply(mat, 1, sd)

  expect_numeric(result, len = nrow(mat))
  expect_equal(result, expected, tolerance = 1e-12)
  # Row names preserved
  expect_equal(names(result), rownames(mat))
})

test_that("parallel_sd matches base R with constant rows (returns 0)", {
  mat <- matrix(5,
    nrow = 4, ncol = 12,
    dimnames = list(paste0("cell", 1:4), month.abb)
  )
  result <- parallel_sd(mat)
  expected <- apply(mat, 1, sd)

  expect_numeric(result, len = nrow(mat))
  expect_equal(result, expected)
  expect_true(all(result == 0))
  expect_equal(names(result), rownames(mat))
})

test_that("parallel_sd fails with non-matrix input", {
  wrong_input <- c(1, 2, 3)
  expect_error(parallel_sd(wrong_input), "Must be of type 'matrix'")
})

test_that("parallel_sd fails with non-numeric matrix", {
  wrong_input <- matrix(letters[1:4], nrow = 2)
  expect_error(parallel_sd(wrong_input), "Must store numerics")
})

test_that("parallel_sd returns NA for rows containing NA (like base R)", {
  mat <- matrix(rnorm(4 * 12), nrow = 4, ncol = 12)
  mat[2, 3] <- NA_real_
  mat[4, 7] <- NA_real_
  result <- parallel_sd(mat)
  expected <- apply(mat, 1, sd) # base R: NA if any NA in row
  expect_numeric(result, len = nrow(mat))
  expect_equal(is.na(result), is.na(expected))
  # where not NA, values should match
  expect_equal(result[!is.na(expected)], expected[!is.na(expected)],
    tolerance = 1e-12
  )
})

test_that(
  "parallel_sd returns NA for rows with < 2 columns (degenerate case)",
  {
    # One column -> sample sd undefined
    mat1 <- matrix(1:5, nrow = 5, ncol = 1)
    res1 <- parallel_sd(mat1)
    exp1 <- apply(mat1, 1, sd)
    expect_true(all(is.na(res1)))
    expect_equal(res1, exp1)
    # Zero columns (empty by columns) is not a valid input matrix for
    # our wrapper; create a matrix with 0 columns and expect an error at
    # assertion.
    mat0 <- matrix(numeric(0), nrow = 3, ncol = 0)
    expect_error(parallel_sd(mat0), regexp = "at least 1 col")
  }
)

test_that("parallel_sd fails with empty matrix", {
  empty_mat <- matrix(numeric(0), nrow = 0, ncol = 0)
  expect_error(parallel_sd(empty_mat), "Must have at least 1 row")
})

test_that("parallel_sd handles Infs similarly to base R", {
  mat <- matrix(
    c(
      1, 2, 3, Inf,
      4, 5, 6, 7,
      Inf, -Inf, 0, 1
    ),
    nrow = 3, byrow = TRUE
  )
  res <- parallel_sd(mat)
  exp <- apply(mat, 1, sd)

  # base R may produce NA/NaN in some Inf combinations; just compare behavior
  expect_equal(is.na(res), is.na(exp))
  finite_idx <- is.finite(exp)
  expect_equal(res[finite_idx], exp[finite_idx], tolerance = 1e-12)
})

test_that("parallel_sd works with large tall matrix (smoke test)", {
  set.seed(42)
  nr <- 2000
  nc <- 12
  mat <- matrix(rnorm(nr * nc), nrow = nr, ncol = nc)
  res <- parallel_sd(mat)
  exp <- apply(mat, 1, sd)
  expect_numeric(res, len = nr)
  # Use a bit looser tolerance due to accumulation order differences
  expect_equal(res, exp, tolerance = 1e-10)
})
