library(testthat)
library(terra)
library(checkmate)

test_that("bio_04 computes temperature seasonality (SD * 100) correctly", {
  tas <- mock_tas()

  result <- bio_04(tas)

  # Output should be SpatRaster with one layer
  expect_s4_class(result, "SpatRaster")
  expect_equal(nlyr(result), 1)

  # Expected: the standard deviation of sequence 1 to 12 times 100
  expected_val <- sd(1:12) * 100
  vals <- values(result)

  expect_true(all(abs(vals - expected_val) < 1e-10))
  # Optional: layer name consistency
  expect_identical(names(result), "bio_04")
})

test_that("bio_04 fails with wrong number of layers", {
  tas <- mock_tas()
  tas_wrong <- tas[[1:6]] # only 6 layers
  expect_error(bio_04(tas_wrong), "tas must have 12 layers")
})

test_that("bio_04 fails with wrong class", {
  wrong_input <- matrix(1:12, nrow = 3)
  expect_error(bio_04(wrong_input), "Must inherit from class 'SpatRaster'")
})

test_that("bio_04 fails with invalid filename", {
  tas <- mock_tas()
  expect_error(bio_04(tas, filename = 123), "Must be of type 'string'")
})
