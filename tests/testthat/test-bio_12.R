library(testthat)
library(checkmate)
library(terra)

test_that("bio_12 computes annual precipitation (sum over 12 months)", {
  # Build a small 2x2 raster with 12 layers of simple integers
  r_base <- rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    crs = "EPSG:4326"
  )
  set.seed(123)
  layers <- lapply(1:12, function(i) setValues(r_base, i)) # layer i has all cells = i
  pr <- rast(layers)
  names(pr) <- paste0("m", 1:12)

  # Expected sum per cell: 1+2+...+12 = 78
  out <- bio_12(pr, filename = "", na_rm = FALSE)

  expect_s4_class(out, "SpatRaster")
  expect_equal(nlyr(out), 1)
  expect_identical(names(out), "bio_12")
  expect_true(all(values(out)[, 1] == 78))
  # Geometry should be preserved
  expect_equal(ext(out), ext(pr))
  expect_identical(crs(out), crs(pr))
  expect_equal(res(out), res(pr))
})


test_that("bio_12 NA handling: na_rm = FALSE propagates NA", {
  # One cell raster with 12 months; insert NA in one month
  r_base <- rast(nrows = 1, ncols = 1)
  vals <- as.list(1:12)
  vals[[5]] <- NA_real_
  layers <- mapply(function(i, v) setValues(r_base, v), i = 1:12, v = vals, SIMPLIFY = FALSE)
  pr <- rast(layers)

  out <- bio_12(pr, wrap = FALSE, filename = "", na_rm = FALSE)
  expect_true(is.na(values(out)[1, 1]))
})

test_that("bio_12 NA handling: na_rm = TRUE sums non-NA, NA if all NA", {
  r_base <- rast(nrows = 1, ncols = 2)
  # col1: months 1..12 but month 5 is NA -> sum = 78 - 5 = 73
  v1 <- 1:12
  v1[5] <- NA_real_
  # col2: all NA -> result NA
  v2 <- rep(NA_real_, 12)

  # Build 12 layers
  layers <- lapply(1:12, function(i) {
    setValues(r_base, c(v1[i], v2[i]))
  })
  pr <- rast(layers)

  out <- bio_12(pr, wrap = FALSE, filename = "", na_rm = TRUE)
  vals <- values(out)[, 1]
  expect_equal(vals[1], 78 - 5)
  expect_true(is.na(vals[2]))
})

test_that("bio_12 writes to disk when filename is provided", {
  r_base <- rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  layers <- lapply(1:12, function(i) setValues(r_base, i))
  pr <- rast(layers)

  tf <- tempfile(fileext = ".tif")
  on.exit(unlink(tf), add = TRUE)
  out <- bio_12(pr, filename = tf, na_rm = FALSE)

  expect_true(file.exists(tf))
  # Reopen from disk and verify values
  out_disk <- rast(tf)
  expect_equal(values(out), values(out_disk))
  expect_identical(names(out_disk), "bio_12")
})

test_that("bio_12 input validation: class and layer count", {
  # wrong class
  expect_error(bio_12(matrix(1:12, nrow = 3)), "must inherit from class")
  # not 12 layers
  r_base <- rast(nrows = 1, ncols = 1)
  pr_bad <- rast(list(setValues(r_base, 1), setValues(r_base, 2))) # 2 layers
  expect_error(bio_12(pr_bad), "must have 12 layers")
})

test_that("bio_12 works on non-trivial values", {
  # 2x2 raster with varying monthly values
  r_base <- rast(nrows = 2, ncols = 2)
  set.seed(42)
  mats <- replicate(12, matrix(sample(0:10, 4, replace = TRUE), nrow = 2), simplify = FALSE)
  layers <- lapply(mats, function(m) setValues(r_base, as.vector(m)))
  pr <- rast(layers)

  # Expected: cell-wise sum across 12 layers
  # Build a 4 x 12 matrix of cell values (cells as rows, months as cols)
  cell_by_month <- do.call(cbind, lapply(1:12, function(i) values(pr[[i]])[, 1]))
  expected <- rowSums(cell_by_month)
  out <- bio_12(pr, wrap = FALSE, filename = "", na_rm = FALSE)
  expect_equal(as.numeric(values(out)[, 1]), expected)
})
