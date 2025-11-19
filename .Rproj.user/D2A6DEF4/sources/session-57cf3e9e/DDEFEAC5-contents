library(testthat)
library(checkmate)


test_that("parallel_average computes row means correctly", {
  mat <- matrix(1:9, nrow = 3)
  result <- parallel_average(mat)

  # Convert result to vector
  result_vec <- as.vector(result)

  expected <- rowMeans(mat)

  expect_numeric(result_vec, len = 3)
  expect_equal(result_vec, expected)
})


test_that("parallel_average fails with non-matrix input", {
  wrong_input <- c(1, 2, 3)
  expect_error(parallel_average(wrong_input), "Must be of type 'matrix'")
})

test_that("parallel_average fails with non-numeric matrix", {
  wrong_input <- matrix(letters[1:4], nrow = 2)
  expect_error(parallel_average(wrong_input), "Must store numerics")
})


test_that("parallel_average fails with NA values", {
  mat_na <- matrix(c(1, NA, 3, 4), nrow = 2)
  expect_error(parallel_average(mat_na), "Contains missing values")
})


test_that("parallel_average fails with empty matrix", {
  empty_mat <- matrix(numeric(0), nrow = 0, ncol = 0)
  expect_error(parallel_average(empty_mat), "Must have at least 1 row")
})
