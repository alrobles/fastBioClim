# tests/testthat/test-bio_01.R
library(testthat)
library(terra)
library(checkmate)

test_that("bio_01 computes annual mean temperature correctly", {
  tas <- mock_tas()
  
  result <- bio_01(tas)
  
  # Output should be SpatRaster with one layer
  expect_s4_class(result, "SpatRaster")
  expect_equal(nlyr(result), 1)
  
  # Expected mean: average of 1:12 = 6.5
  expected_mean <- mean(1:12)
  vals <- values(result)
  
  expect_true(all(vals == expected_mean))
})

test_that("bio_01 fails with wrong number of layers", {
  tas <- mock_tas()
  tas_wrong <- tas[[1:6]] # only 6 layers
  expect_error(bio_01(tas_wrong), "tas must have 12 layers")
})


test_that("bio_01 fails with wrong class", {
  wrong_input <- matrix(1:12, nrow = 3)
  expect_error(bio_01(wrong_input), "Must inherit from class 'SpatRaster'")
})



test_that("bio_01 fails with invalid filename", {
  tas <- mock_tas()
  expect_error(bio_01(tas, filename = 123), "Must be of type 'string'")
})

