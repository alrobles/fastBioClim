library(testthat)
library(checkmate)

test_that("parallel_difference computes element-wise differences correctly", {
  mat1 <- matrix(1:9, nrow = 3)
  mat2 <- matrix(9:1, nrow = 3)
  
  result <- parallel_difference(mat1, mat2)
  
  # Expected result: mat1 - mat2
  expected <- mat1 - mat2
  
  expect_matrix(result, mode = "numeric", nrows = 3, ncols = 3)
  expect_equal(result, expected)
})

test_that("parallel_difference fails with non-matrix input", {
  wrong_input <- c(1, 2, 3)
  expect_error(
    parallel_difference(wrong_input, wrong_input),
    "Must be of type 'matrix'"
  )
})

test_that("parallel_difference fails with non-numeric matrix", {
  wrong_input <- matrix(letters[1:4], nrow = 2)
  expect_error(
    parallel_difference(wrong_input, wrong_input),
    "Must store numerics"
  )
})

test_that("parallel_difference fails with NA values", {
  mat_na <- matrix(c(1, NA, 3, 4), nrow = 2)
  expect_error(parallel_difference(mat_na, mat_na), "Contains missing values")
})

test_that("parallel_difference fails with mismatched dimensions", {
  mat1 <- matrix(1:4, nrow = 2)
  mat2 <- matrix(1:6, nrow = 3)
  expect_error(parallel_difference(mat1, mat2), "must have the same dimensions")
})
