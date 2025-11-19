library(testthat)
library(checkmate)

test_that("basic 3-month averages without wrap, no NA", {
  mat <- matrix(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9), # 3 rows x 3 cols
    nrow = 3, byrow = TRUE,
    dimnames = list(c("r1", "r2", "r3"), NULL)
  )
  idx <- matrix(c(1, 1, 1), ncol = 1) # all start at column 1

  out <- parallel_average_quarter(idx, mat, wrap = FALSE, na_rm = FALSE)

  expect_numeric(out, len = 3)
  expect_equal(out, c(r1 = 2, r2 = 5, r3 = 8))
})

test_that("basic 3-month averages with wrap", {
  mat <- matrix(
    c(
      1, 10, 100,
      2, 20, 200,
      3, 30, 300
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("a", "b", "c"), paste0("m", 1:3))
  )
  # start at col 2: window = col2, col3, col1 (wrap)
  idx <- matrix(c(2, 2, 2), ncol = 1)

  out <- parallel_average_quarter(idx, mat, wrap = TRUE, na_rm = FALSE)

  expect_equal(unname(out["a"]), (10 + 100 + 1) / 3)
  expect_equal(unname(out["b"]), (20 + 200 + 2) / 3)
  expect_equal(unname(out["c"]), (30 + 300 + 3) / 3)
})

test_that("no wrap at end returns NA if window exceeds ncol", {
  mat <- matrix(
    c(
      1, 2, 3, 4,
      5, 6, 7, 8
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("r1", "r2"), NULL)
  )
  idx <- matrix(c(3, 4), ncol = 1) # start at 3 & 4 for a 4-col matrix

  # For r1: start=3 -> needs cols 3,4,5 => invalid => NA
  # For r2: start=4 -> needs cols 4,5,6 => invalid => NA
  out <- parallel_average_quarter(idx, mat, wrap = FALSE, na_rm = FALSE)
  expect_true(is.na(out["r1"]))
  expect_true(is.na(out["r2"]))
})

test_that("wrap at end works (e.g., Dec–Jan–Feb)", {
  mat <- matrix(
    c(
      1, 2, 3, 4,
      10, 20, 30, 40
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("x", "y"), paste0("m", 1:4))
  )
  idx <- matrix(c(3, 4), ncol = 1)

  # x: start=3 -> cols (3,4,1) = (3,4,1)
  # y: start=4 -> cols (4,1,2) = (40,10,20)
  out <- parallel_average_quarter(idx, mat, wrap = TRUE, na_rm = FALSE)
  expect_equal(unname(out["x"]), (3 + 4 + 1) / 3)
  expect_equal(unname(out["y"]), (40 + 10 + 20) / 3)
})

test_that("NA handling: na_rm = FALSE propagates any NA", {
  mat <- matrix(
    c(
      1, NA, 3,
      NA, NA, NA,
      4, 5, 6
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("r1", "r2", "r3"), NULL)
  )
  idx <- matrix(c(1, 1, 1), ncol = 1)

  out <- parallel_average_quarter(idx, mat, wrap = FALSE, na_rm = FALSE)
  expect_true(is.na(out["r1"])) # NA present in window -> NA
  expect_true(is.na(out["r2"])) # all NA -> NA
  expect_equal(unname(out["r3"]), 5) # no NA -> average = 5
})

test_that("NA handling: na_rm = TRUE averages non-NA; NA if all NA", {
  mat <- matrix(
    c(
      1, NA, 3,
      NA, NA, NA,
      4, 5, NA
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("r1", "r2", "r3"), NULL)
  )
  idx <- matrix(c(1, 1, 1), ncol = 1)

  out <- parallel_average_quarter(idx, mat, wrap = FALSE, na_rm = TRUE)
  expect_equal(unname(out["r1"]), (1 + 3) / 2) # average over non-NA only
  expect_true(is.na(out["r2"])) # all NA -> NA
  expect_equal(unname(out["r3"]), (4 + 5) / 2) # average over non-NA only
})

test_that("invalid idx values yield NA (out of range or NA)", {
  mat <- matrix(
    c(
      1, 2, 3,
      4, 5, 6
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("a", "b"), NULL)
  )
  idx <- matrix(c(0, NA_real_), ncol = 1) # 0 invalid, NA invalid

  out <- parallel_average_quarter(idx, mat, wrap = TRUE, na_rm = TRUE)
  expect_true(is.na(out["a"]))
  expect_true(is.na(out["b"]))
})

test_that("input validation: shape and row count checks", {
  mat <- matrix(1:6, nrow = 2, byrow = TRUE)
  good_idx <- matrix(c(1, 1), ncol = 1)
  bad_idx_ncol2 <- matrix(c(1, 1, 2, 2), ncol = 2)
  bad_idx_nrow <- matrix(1, ncol = 1)

  # wrong ncols for idx
  expect_error(parallel_average_quarter(bad_idx_ncol2, mat))
  # wrong nrows for idx vs mat
  expect_error(parallel_average_quarter(bad_idx_nrow, mat))
  # mat with < 3 cols
  mat_small <- matrix(1:4, nrow = 2, byrow = TRUE) # only 2 cols
  expect_error(parallel_average_quarter(good_idx, mat_small))
})

test_that("row names are propagated to output", {
  mat <- matrix(
    c(
      1, 2, 3,
      4, 5, 6
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("rowA", "rowB"), NULL)
  )
  idx <- matrix(c(1, 1), ncol = 1)

  out <- parallel_average_quarter(idx, mat, wrap = FALSE, na_rm = FALSE)
  expect_identical(names(out), rownames(mat))
})

test_that("works with larger matrices and mixed wrap behaviors", {
  set.seed(123)
  mat <- matrix(runif(5 * 12),
    nrow = 5, ncol = 12,
    dimnames = list(paste0("r", 1:5), paste0("m", 1:12))
  )
  # start months per row
  idx <- matrix(c(1, 12, 5, 9, 3), ncol = 1)

  # Compute expected manually for a couple of rows
  # r1, start=1, no wrap needed
  exp_r1 <- mean(mat[1, 1:3])
  # r2, start=12, wrap -> (12, 1, 2)
  exp_r2 <- mean(c(mat[2, 12], mat[2, 1], mat[2, 2]))

  out <- parallel_average_quarter(idx, mat, wrap = TRUE, na_rm = FALSE)
  expect_equal(unname(out["r1"]), exp_r1)
  expect_equal(unname(out["r2"]), exp_r2)
})
