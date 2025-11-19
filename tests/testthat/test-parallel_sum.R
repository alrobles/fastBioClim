library(testthat)
library(checkmate)

test_that("parallel_sum computes row sums correctly", {
  mat <- matrix(1:9, nrow = 3, byrow = TRUE)
  # Expected sums: row1 = 1+2+3 = 6; row2 = 4+5+6 = 15; row3 = 7+8+9 = 24
  expected <- rowSums(mat)

  result <- parallel_sum(mat)
  result_vec <- as.vector(result)

  expect_numeric(result_vec, len = 3)
  expect_equal(result_vec, expected)
})

test_that("parallel_sum works with negative and zero values", {
  mat <- matrix(c(-1, 0, 1, 2, -2, 3), nrow = 2, byrow = TRUE)
  expected <- rowSums(mat)

  result <- parallel_sum(mat)
  expect_equal(as.vector(result), expected)
})

test_that("parallel_sum works with NA values (should propagate NA)", {
  mat <- matrix(c(1, NA, 3, 4), nrow = 2, byrow = TRUE)
  # rowSums with na.rm = FALSE propagates NA
  expected <- rowSums(mat, na.rm = FALSE)

  result <- parallel_sum(mat)
  expect_equal(as.vector(result), expected)
})

test_that("parallel_sum fails with non-matrix input", {
  wrong_input <- c(1, 2, 3)
  expect_error(parallel_sum(wrong_input), "Must be of type 'matrix'")
})

test_that("parallel_sum fails with non-numeric matrix", {
  wrong_input <- matrix(letters[1:4], nrow = 2)
  expect_error(parallel_sum(wrong_input), "Must store numerics")
})
