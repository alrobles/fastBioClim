library(testthat)
library(terra)
library(checkmate)

test_that("bio_06 computes min temperature of coldest month correctly", {
  tasmin <- mock_tas()

  result <- bio_06(tasmin)

  # Output should be SpatRaster with one layer
  expect_s4_class(result, "SpatRaster")
  expect_equal(nlyr(result), 1)

  # Expected: min of 1:12 = 1
  expected_val <- min(1:12)
  vals <- values(result)

  expect_true(all(abs(vals - expected_val) < 1e-10))
  # Optional: layer name consistency
  expect_identical(names(result), "bio_06")
})

test_that("bio_06 fails with wrong number of layers", {
  tasmin <- mock_tas()
  tasmin_wrong <- tasmin[[1:6]] # only 6 layers
  expect_error(bio_06(tasmin_wrong), "tasmin must have 12 layers")
})

test_that("bio_06 fails with wrong class", {
  wrong_input <- matrix(1:12, nrow = 3)
  expect_error(bio_06(wrong_input), "Must inherit from class 'SpatRaster'")
})

test_that("bio_06 fails with invalid filename", {
  tasmin <- mock_tas()
  expect_error(bio_06(tasmin, filename = 123), "Must be of type 'string'")
})
