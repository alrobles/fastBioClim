library(testthat)
library(terra)
library(checkmate)

test_that("bio_05 computes max temperature of warmest month correctly", {
  tasmax <- mock_tas()

  result <- bio_05(tasmax)

  # Output should be SpatRaster with one layer
  expect_s4_class(result, "SpatRaster")
  expect_equal(nlyr(result), 1)

  # Expected: max of 1:12 = 12
  expected_val <- max(1:12)
  vals <- values(result)

  expect_true(all(abs(vals - expected_val) < 1e-10))
  # Optional: layer name consistency
  expect_identical(names(result), "bio_05")
})

test_that("bio_05 fails with wrong number of layers", {
  tasmax <- mock_tas()
  tasmax_wrong <- tasmax[[1:6]] # only 6 layers
  expect_error(bio_05(tasmax_wrong), "tasmax must have 12 layers")
})

test_that("bio_05 fails with wrong class", {
  wrong_input <- matrix(1:12, nrow = 3)
  expect_error(bio_05(wrong_input), "Must inherit from class 'SpatRaster'")
})

test_that("bio_05 fails with invalid filename", {
  tasmax <- mock_tas()
  expect_error(bio_05(tasmax, filename = 123), "Must be of type 'string'")
})
