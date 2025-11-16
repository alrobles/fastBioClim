library(testthat)
library(terra)
library(checkmate)

test_that("bio_02 computes mean diurnal temperature range correctly", {
  # values same as month number
  tasmax <- mock_tas()
  # values same as month number (same for simplicity)
  tasmin <- mock_tas()
  
  result <- bio_02(tasmax, tasmin)
  
  # Output should be SpatRaster with one layer
  expect_s4_class(result, "SpatRaster")
  expect_equal(nlyr(result), 1)
  
  # Expected difference: mean(tasmax) - mean(tasmin) = 6.5 - 6.5 = 0
  vals <- values(result)
  expect_true(all(vals == 0))
})

test_that("bio_02 fails with wrong number of layers", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()[[1:6]] # only 6 layers
  expect_error(bio_02(tasmax, tasmin), "tasmin must have 12 layers")
})

test_that("bio_02 fails with wrong class", {
  wrong_input <- matrix(1:12, nrow = 3)
  tasmax <- mock_tas()
  expect_error(
    bio_02(wrong_input, tasmax),
    "Must inherit from class 'SpatRaster'"
  )
})

test_that("bio_02 fails with mismatched dimensions", {
  tasmax <- mock_tas()
  tasmin <- terra::crop(mock_tas(), ext(-90, 90, -45, 45)) # smaller extent
  expect_error(bio_02(tasmax, tasmin), "must have same spatial dimensions")
})

test_that("bio_02 fails with invalid filename", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()
  expect_error(
    bio_02(tasmax, tasmin, filename = 123),
    "Must be of type 'string'"
  )
})
