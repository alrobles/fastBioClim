library(testthat)
library(checkmate)

# Helper to make 1-col index matrix (1-based)
ix <- function(v) matrix(as.numeric(v), ncol = 1)

test_that("computes averages correctly without wrap", {
  # 3 rows, 12 cols; row i is 1..12 + offset
  m <- matrix(rep(1:12, 3), nrow = 3, byrow = TRUE)
  # start index 10 => window (10,11,12) => average of (10,11,12) = 11
  idx <- ix(rep(10, nrow(m)))
  out <- parallel_average_quarter(idx, m, wrap = FALSE)
  expect_numeric(out, any.missing = FALSE, len = nrow(m))
  expect_equal(as.vector(out), rep(mean(c(10, 11, 12)), 3))
})

test_that("computes averages correctly with wrap", {
  # Row 1: numbers 1..12
  # start=12 with wrap => (12,1,2) => avg = (12+1+2)/3 = 5
  m <- matrix(1:12, nrow = 1)
  idx <- ix(12)
  out <- parallel_average_quarter(idx, m, wrap = TRUE)
  expect_equal(as.vector(out), (12 + 1 + 2) / 3)
  
  # Multi-row: different starts
  m2 <- rbind(1:12, 11:22, 101:112)  # 3 rows
  idx2 <- ix(c(12, 11, 1))
  # Row1 (12,1,2) -> (12+1+2)/3
  # Row2 (11,12,13) -> (11+12+13)/3
  # Row3 (1,2,3) in that row's scale -> (101+102+103)/3, but start=1 -> (101,102,103)
  out2 <- parallel_average_quarter(idx2, m2, wrap = TRUE)
  expect_equal(
    as.vector(out2),
    c((12 + 1 + 2) / 3, (21 + 22 + 11) / 3, (101 + 102 + 103) / 3)
  )
})

test_that("ignores NA values in the 3-month window; all-NA yields NA", {
  # Row1: (10, NA, 12) -> avg = (10+12)/2 = 11 (no wrap)
  # Row2: (NA, NA, 5) -> avg = 5
  # Row3: (NA, NA, NA) -> NA
  m <- rbind(
    c(1:9, 10, NA, 12),
    c(NA, NA, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2),
    rep(NA_real_, 12)
  )
  idx <- ix(c(10, 1, 5))
  out <- parallel_average_quarter(idx, m, wrap = FALSE)
  expect_equal(out[[1]], (10 + 12) / 2)
  expect_equal(out[[2]], 5)
  expect_true(is.na(out[[3]]))
})

test_that("wrap affects windows near the end correctly", {
  m <- matrix(1:12, nrow = 1)
  # wrap=FALSE, start=11 -> (11,12,13) invalid -> NA
  out_nowrap <- parallel_average_quarter(ix(11), m, wrap = FALSE)
  expect_true(is.na(out_nowrap[[1]]))
  
  # wrap=TRUE, start=11 -> (11,12,1) -> avg = (11+12+1)/3 = 8
  out_wrap <- parallel_average_quarter(ix(11), m, wrap = TRUE)
  expect_equal(out_wrap[[1]], (11 + 12 + 1) / 3)
})

test_that("preserves row names", {
  m <- matrix(1:18, nrow = 3, byrow = TRUE)
  rownames(m) <- c("cell_A", "cell_B", "cell_C")
  out <- parallel_average_quarter(ix(rep(1, 3)), m, wrap = FALSE)
  expect_named(out, rownames(m))
})

test_that("fails with non-matrix input", {
  wrong_idx <- c(1, 2, 3)
  m <- matrix(1:12, nrow = 3, byrow = TRUE)
  expect_error(parallel_average_quarter(wrong_idx, m), "Must be of type 'matrix'")
})


test_that("fails when idx has wrong number of columns", {
  m <- matrix(1:12, nrow = 3, byrow = TRUE)
  bad_idx <- matrix(1:6, nrow = 3, ncol = 2)
  expect_error(
    parallel_average_quarter(bad_idx, m),
    regexp = "exactly\\s*1\\s*cols",
    ignore.case = TRUE
  )
})

test_that("fails when idx rows != mat rows", {
  m <- matrix(1:12, nrow = 3, byrow = TRUE)
  bad_idx <- ix(rep(1, 2))
  expect_error(parallel_average_quarter(bad_idx, m), "same number of rows")
})

test_that("fails with non-numeric matrix", {
  m <- matrix(letters[1:6], nrow = 2)
  idx <- ix(c(1, 1))
  expect_error(parallel_average_quarter(idx, m), "Must store numerics")
})

test_that("fails with fewer than 3 columns", {
  m <- matrix(as.numeric(1:4), nrow = 2, ncol = 2)
  idx <- ix(c(1, 1))
  expect_error(parallel_average_quarter(idx, m))
})

test_that("invalid wrap flag errors", {
  m <- matrix(1:12, nrow = 3, byrow = TRUE)
  idx <- ix(rep(1, 3))
  expect_error(parallel_average_quarter(idx, m, wrap = "yes"), regexp = "logical flag")
})

test_that("index out of bounds returns NA (consistent with C++)", {
  m <- matrix(1:12, nrow = 2, byrow = TRUE)
  # start=0 and start=13 are invalid -> NA regardless of wrap
  idx <- ix(c(0, 13))
  out_nowrap <- parallel_average_quarter(idx, m, wrap = FALSE)
  out_wrap   <- parallel_average_quarter(idx, m, wrap = TRUE)
  expect_true(all(is.na(out_nowrap)))
  expect_true(all(is.na(out_wrap)))
})

test_that("works with a single row and many columns", {
  m <- matrix(c(0, 0, 0, 10, 10, 10, 5, 5, 5), nrow = 1)
  idx <- ix(4)  # (10,10,10) -> avg 10
  out <- parallel_average_quarter(idx, m, wrap = FALSE)
  expect_equal(as.vector(out), 10)
})

test_that("handles NA index as NA output", {
  m <- matrix(1:12, nrow = 2, byrow = TRUE)
  idx <- ix(c(NA_real_, 1))
  out <- parallel_average_quarter(idx, m, wrap = FALSE)
  expect_true(is.na(out[[1]]))
  expect_false(is.na(out[[2]]))
})

