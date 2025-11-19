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

test_that("bio_08 returns a single-layer SpatRaster named 'bio_08'", {
  pr <- mock_pr() # 12-layer monthly precipitation
  tas <- mock_tas() # 12-layer monthly mean temperature

  out <- bio_08(pr, tas, wrap = FALSE)

  expect_s4_class(out, "SpatRaster")
  expect_equal(nlyr(out), 1)
  expect_identical(names(out), "bio_08")
})

test_that("bio_08 computes mean temperature of the wettest quarter (no wrap)", {
  # Assumptions:
  # mock_pr() -> increasing monthly pr 1..12
  # mock_tas() -> 1..12
  # Wettest non-wrapping quarter months 10:12; mean(10,11,12) = 11
  pr <- mock_pr()
  tas <- mock_tas()

  out <- bio_08(pr, tas, wrap = FALSE)
  vals <- terra::values(out)

  expect_true(all(abs(vals - 11) < 1e-10))
})

test_that("bio_08 respects wrap-around when Dec–Jan–Feb is wettest quarter", {
  # pr: Dec, Jan, Feb very wet -> wrap=TRUE picks Dec–Jan–Feb
  # Without wrap, the max non-wrapping window is Jan–Mar.
  pr_vals <- c(100, 100, 1, 1, 1, 1, 1, 1, 1, 1, 1, 100) # Jan..Dec
  tas_vals <- 1:12

  pr <- make_1x1_12(pr_vals)
  tas <- make_1x1_12(tas_vals)

  out_nowrap <- bio_08(pr, tas, wrap = FALSE) # picks Jan–Mar -> mean = 2
  out_wrap <- bio_08(pr, tas, wrap = TRUE) # picks Dec–Jan–Feb -> mean = 5

  expect_equal(as.numeric(terra::values(out_nowrap)), 2)
  expect_equal(as.numeric(terra::values(out_wrap)), 5)
})

# --- NA handling with na_rm = TRUE (robust behavior) ---
test_that("bio_08 with na_rm=TRUE averages non-NA; all-NA yields NA", {
  # Make the wettest quarter Jan–Mar via precipitation
  pr_vals <- c(10, 10, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1)

  # Case 1: tas has NA, NA, 3 in Jan–Mar -> average over non-NA = 3
  tas_vals1 <- c(NA, NA, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  # Case 2: tas has all NA in Jan–Mar -> result NA
  tas_vals2 <- c(NA, NA, NA, 4, 5, 6, 7, 8, 9, 10, 11, 12)

  pr <- make_1x1_12(pr_vals)
  tas1 <- make_1x1_12(tas_vals1)
  tas2 <- make_1x1_12(tas_vals2)

  out1 <- bio_08(pr, tas1, wrap = FALSE, na_rm = TRUE)
  out2 <- bio_08(pr, tas2, wrap = FALSE, na_rm = TRUE)

  expect_equal(as.numeric(terra::values(out1)), 3)
  expect_true(is.na(terra::values(out2)))
})

# --- Optional: document default behavior na_rm = FALSE (propagate NA) ---
test_that("bio_08 with na_rm=FALSE propagates NA in the selected window", {
  pr_vals <- c(10, 10, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1) # wettest Jan–Mar
  tas_vals <- c(NA, NA, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) # NA present in window
  pr <- make_1x1_12(pr_vals)
  tas <- make_1x1_12(tas_vals)

  out <- bio_08(pr, tas, wrap = FALSE, na_rm = FALSE)
  expect_true(is.na(terra::values(out)))
})

test_that("bio_08 fails with wrong number of layers in pr", {
  pr <- mock_pr()
  tas <- mock_tas()
  pr_wrong <- pr[[1:6]] # only 6 layers
  expect_error(bio_08(pr_wrong, tas), "pr must have 12 layers")
})

test_that("bio_08 fails with wrong number of layers in tas", {
  pr <- mock_pr()
  tas <- mock_tas()
  tas_wrong <- tas[[1:6]] # only 6 layers
  expect_error(bio_08(pr, tas_wrong), "tas must have 12 layers")
})

test_that("bio_08 fails when pr and tas have different dimensions", {
  pr <- mock_pr()
  tas <- mock_tas()
  # Drop one row from tas to change spatial dimensions
  tas_small <- tas[
    seq_len(terra::nrow(tas) - 1),
    seq_len(terra::ncol(tas)), ,
    drop = FALSE
  ]
  expect_error(
    bio_08(pr, tas_small),
    "pr and tas must have same dimensions"
  )
})

test_that("bio_08 fails with wrong class inputs", {
  pr <- mock_pr()
  wrong_input <- matrix(1:12, nrow = 3)
  expect_error(bio_08(wrong_input, pr), "Must inherit from class 'SpatRaster'")
  expect_error(bio_08(pr, wrong_input), "Must inherit from class 'SpatRaster'")
})

test_that("bio_08 fails with invalid filename", {
  pr <- mock_pr()
  tas <- mock_tas()
  expect_error(bio_08(pr, tas, filename = 123), "Must be of type 'string'")
})

test_that("bio_08 can write to a file", {
  pr <- mock_pr()
  tas <- mock_tas()
  f <- tempfile(fileext = ".tif")
  out <- bio_08(pr, tas, filename = f)
  expect_true(file.exists(f))
  expect_s4_class(out, "SpatRaster")
  expect_equal(nlyr(out), 1)
  expect_identical(names(out), "bio_08")
})
