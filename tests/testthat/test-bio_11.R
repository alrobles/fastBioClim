# test-bio_11.R
library(testthat)
library(terra)
library(checkmate)

# Helper to build 1x1, 12-layer rasters with supplied monthly values
make_1x1_12 <- function(vals) {
  stopifnot(length(vals) == 12)
  r <- terra::rast(nrows = 1, ncols = 1, nlyrs = 12)
  terra::values(r) <- matrix(vals, nrow = 1)
  r
}

test_that("bio_11 returns a single-layer SpatRaster named 'bio_11'", {
  tas <- mock_tas() # 12-layer monthly mean temperature

  out <- bio_11(tas, wrap = FALSE)

  expect_s4_class(out, "SpatRaster")
  expect_equal(nlyr(out), 1)
  expect_identical(names(out), "bio_11")
})

test_that("bio_11 computes mean temperature of the coldest quarter (no wrap)", {
  # Assumptions:
  # mock_tas() -> increasing monthly tas 1..12
  # Coldest non-wrapping quarter is months 1:3; mean(1,2,3) = 2
  tas <- mock_tas()

  out <- bio_11(tas, wrap = FALSE)
  vals <- terra::values(out)

  expect_true(all(abs(vals - 2) < 1e-10))
})

test_that("bio_11 respects wrap-around when Dec–Jan–Feb is coldest quarter", {
  # tas (Jan..Dec): make Dec, Jan, Feb the coldest triad
  # wrap TRUE: Dec–Jan–Feb -> mean = (-50 + -50 + -120)/3 = -220/3
  # wrap FALSE: best non-wrap is Jan–Mar (-50,-50,1) -> mean = (-99)/3
  tas_vals <- c(-50, -50, 1, 1, 1, 1, 1, 1, 1, 1, 1, -120)
  tas <- make_1x1_12(tas_vals)

  out_nowrap <- bio_11(tas, wrap = FALSE)
  out_wrap <- bio_11(tas, wrap = TRUE)

  expect_equal(as.numeric(terra::values(out_nowrap)), (1 + 1 - 120) / 3)
  expect_equal(as.numeric(terra::values(out_wrap)), (-50 - 50 - 120) / 3)
})

test_that("bio_11 with na_rm=TRUE averages non-NA; all-NA year yields NA", {
  # Case 1: Coldest quarter Jan–Mar contains NA, NA, -30 so average over
  # non-NA is -30
  tas_vals1 <- c(NA, NA, -30, 1, 1, 1, 1, 1, 1, 1, 1, 1)

  # Case 2: All months NA -> every 3-month window invalid -> result NA
  tas_vals2 <- rep(NA_real_, 12)

  tas1 <- make_1x1_12(tas_vals1)
  tas2 <- make_1x1_12(tas_vals2)

  out1 <- bio_11(tas1, wrap = FALSE, na_rm = TRUE)
  out2 <- bio_11(tas2, wrap = FALSE, na_rm = TRUE)

  expect_equal(as.numeric(terra::values(out1)), -30)
  expect_true(is.na(terra::values(out2)))
})

test_that("bio_11 with na_rm=FALSE returns NA when all windows contain NA", {
  # Construct a pattern where every 3-month window has at least one NA
  tas_vals <- rep(c(NA_real_, 1), length.out = 12) # NA,1,NA,1,...
  tas <- make_1x1_12(tas_vals)

  out <- bio_11(tas, wrap = FALSE, na_rm = FALSE)
  expect_true(is.na(terra::values(out)))
})

test_that("bio_11 fails with wrong number of layers in tas", {
  tas <- mock_tas()
  tas_wrong <- tas[[1:6]] # only 6 layers
  expect_error(bio_11(tas_wrong), "tas must have 12 layers")
})

test_that("bio_11 fails with wrong class input", {
  wrong_input <- matrix(1:12, nrow = 3)
  expect_error(bio_11(wrong_input), "Must inherit from class 'SpatRaster'")
})

test_that("bio_11 fails with invalid filename", {
  tas <- mock_tas()
  expect_error(bio_11(tas, filename = 123), "Must be of type 'string'")
})

test_that("bio_11 can write to a file", {
  tas <- mock_tas()
  f <- tempfile(fileext = ".tif")
  out <- bio_11(tas, filename = f)

  expect_true(file.exists(f))
  expect_s4_class(out, "SpatRaster")
  expect_equal(nlyr(out), 1)
  expect_identical(names(out), "bio_11")
})
