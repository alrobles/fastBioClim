library(testthat)
library(terra)
library(checkmate)

test_that("bio_03 computes Isothermality correctly on simple mock data", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()

  # With tasmax == tasmin == (1..12), BIO2 = mean(m - m) = 0, BIO7 = 12 - 1 = 11
  # => BIO3 = 100 * 0 / 11 = 0
  result <- bio_03(tasmax, tasmin)

  # Output should be SpatRaster with one layer named bio_03
  expect_s4_class(result, "SpatRaster")
  expect_equal(nlyr(result), 1)
  expect_identical(names(result), "bio_03")

  vals <- values(result)
  expect_true(all(abs(vals - 0) < 1e-10))
})

test_that("bio_03 respects the 100x scaling and produces expected percentage", {
  tasmax <- mock_tas()
  # Construct tasmin such that tasmax - tasmin = 2 everywhere (BIO2 = 2)
  # and min(tasmin) = 1 - 2 = -1, so BIO7 = 12 - (-1) = 13
  # => BIO3 = 100 * 2 / 13
  tasmin2 <- tasmax
  terra::values(tasmin2) <- terra::values(tasmax) - 2

  result <- bio_03(tasmax, tasmin2)

  expected_val <- 100 * 2 / 13
  vals <- values(result)
  expect_true(all(abs(vals - expected_val) < 1e-10))
  expect_identical(names(result), "bio_03")
})

test_that("bio_03 sets NA when BIO7 is zero (division-by-zero safe guard)", {
  # Create constant stacks where all months have the same value
  base <- mock_tas()[[1]]
  base_const <- base * 0 + 5

  # Build a 12-layer constant SpatRaster
  layers <- lapply(1:12, function(i) base_const)
  tas_const <- terra::rast(layers)

  # tasmax == tasmin == constant -> BIO7 = max - min = 0 -> BIO3 should be NA
  res <- bio_03(tas_const, tas_const)
  vals <- values(res)

  expect_true(all(is.na(vals)))
  expect_identical(nlyr(res), 1)
  expect_identical(names(res), "bio_03")
})

test_that("bio_03 fails with wrong number of layers in tasmax", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()
  tasmax_wrong <- tasmax[[1:6]] # only 6 layers
  expect_error(bio_03(tasmax_wrong, tasmin), "tasmax must have 12 layers")
})

test_that("bio_03 fails with wrong number of layers in tasmin", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()
  tasmin_wrong <- tasmin[[1:6]] # only 6 layers
  expect_error(bio_03(tasmax, tasmin_wrong), "tasmin must have 12 layers")
})

test_that("bio_03 fails when tasmax and tasmin have different dimensions", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()

  # Make tasmin have different spatial dimensions (drop one row)
  tasmin_small <- tasmin[
    seq_len((terra::nrow(tasmin) - 1)),
    seq_len(terra::ncol(tasmin)), ,
    drop = FALSE
  ]

  expect_error(
    bio_03(tasmax, tasmin_small),
    # Either your explicit message or the compareGeom message:
    # "tasmax and tasmin must have same spatial dimensions"
    # but since bio_03 uses compareGeom with stopOnError=TRUE, match a part of
    # that:
    "different geometry|extent|resolution|CRS",
    ignore.case = TRUE
  )
})

test_that("bio_03 fails with wrong class inputs", {
  tasmax <- mock_tas()
  wrong_input <- matrix(1:12, nrow = 3)

  expect_error(
    bio_03(wrong_input, tasmax),
    "Must inherit from class 'SpatRaster'"
  )
  expect_error(
    bio_03(tasmax, wrong_input),
    "Must inherit from class 'SpatRaster'"
  )
})

test_that("bio_03 fails with invalid filename", {
  tasmax <- mock_tas()
  tasmin <- mock_tas()
  expect_error(
    bio_03(tasmax, tasmin, filename = 123),
    "Must be of type 'string'"
  )
})
