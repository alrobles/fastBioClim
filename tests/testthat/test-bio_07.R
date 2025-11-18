library(testthat)
library(terra)
library(checkmate)

test_that("bio_07 computes temperature annual range correctly", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()

  result <- bio_07(tasmax, tasmin)

  # Output should be SpatRaster with one layer
  expect_s4_class(result, "SpatRaster")
  expect_equal(nlyr(result), 1)

  # Expected: the max of  1 to 12  minus the min of 1 to 12
  # equals 11
  expected_val <- max(1:12) - min(1:12)
  vals <- values(result)

  expect_true(all(abs(vals - expected_val) < 1e-10))
  # Optional: layer name consistency
  expect_identical(names(result), "bio_07")
})

test_that("bio_07 fails with wrong number of layers in tasmax", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()
  tasmax_wrong <- tasmax[[1:6]] # only 6 layers
  expect_error(bio_07(tasmax_wrong, tasmin), "tasmax must have 12 layers")
})

test_that("bio_07 fails with wrong number of layers in tasmin", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()
  tasmin_wrong <- tasmin[[1:6]] # only 6 layers
  expect_error(bio_07(tasmax, tasmin_wrong), "tasmin must have 12 layers")
})


test_that("bio_07 fails when tasmax and tasmin have different dimensions", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()

  # Make tasmin have different spatial dimensions (drop one row)
  tasmin_small <- tasmin[
    seq_len((terra::nrow(tasmin) - 1)),
    seq_len(terra::ncol(tasmin)), ,
    drop = FALSE
  ]
  expect_error(
    bio_07(tasmax, tasmin_small),
    "tasmax and tasmin must have same spatial dimensions"
  )
})


test_that("bio_07 fails with wrong class inputs", {
  tasmax <- mock_tas()
  wrong_input <- matrix(1:12, nrow = 3)

  expect_error(
    bio_07(wrong_input, tasmax),
    "Must inherit from class 'SpatRaster'"
  )
  expect_error(
    bio_07(tasmax, wrong_input),
    "Must inherit from class 'SpatRaster'"
  )
})

test_that("bio_07 fails with invalid filename", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()
  expect_error(
    bio_07(tasmax, tasmin, filename = 123),
    "Must be of type 'string'"
  )
})
