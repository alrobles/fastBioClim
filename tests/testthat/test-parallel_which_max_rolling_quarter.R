library(testthat)
library(checkmate)

test_that("returns correct starting indices without wrap", {
  # Simple increasing rows: max 3-month sum is at the last possible non-wrap start
  m1 <- matrix(1:(12*3), nrow = 3, byrow = TRUE)
  idx <- parallel_which_max_rolling_quarter(m1, wrap = FALSE, na_rm = FALSE)
  
  expect_integer(idx, any.missing = FALSE, len = nrow(m1))
  expect_equal(as.vector(idx), rep(10L, 3))  # windows: 10-12
})


test_that("returns correct starting indices with wrap", {
  # r1: wrap (12,1,2) is strictly maximal
  r1 <- c(90, 90, rep(10, 7), 10, 10, 120)  # months 1,2,12 large; 10,11 small
  # r2: also maximal at wrap
  r2 <- c(100, 100, rep(10, 9), 100)
  # r3: maximal non-wrap in the middle (start=5)
  r3 <- c(1,1,1,1, 100,100,100, 1,1,1,1,1)
  
  m2 <- rbind(r1, r2, r3)
  idx <- parallel_which_max_rolling_quarter(m2, wrap = TRUE, na_rm = FALSE)
  
  expect_equal(as.vector(idx), c(12L, 12L, 5L))
})



test_that("handles NA with na_rm = FALSE (any NA in row -> NA)", {
  m <- rbind(
    c(1, 2, 3, 4, 5, 6),
    c(1, NA, 3, 4, 5, 6),  # has NA somewhere
    c(NA, NA, NA, 1, 1, 1) # has NA
  )
  idx <- parallel_which_max_rolling_quarter(m, wrap = FALSE, na_rm = FALSE)
  expect_true(is.na(idx[2]))
  expect_true(is.na(idx[3]))
  expect_false(is.na(idx[1]))
})

test_that("handles NA with na_rm = TRUE (skip NA windows)", {
  # Row 1: only one clean max window -> expect its start
  r1 <- c(1, 2, NA, 100, 100, 100)   # windows with NA are skipped; max clean is 4-6 -> start=4
  # Row 2: all windows contain NA -> result should be NA
  r2 <- c(NA, 1, NA, 2, NA, 3)
  # Row 3: wrap allowed; a wrap window is clean and best -> expect 6 (months 6,1,2)
  r3 <- c(50, 50, 50, 1, 1, 100)
  
  m <- rbind(r1, r2, r3)
  idx_nowrap <- parallel_which_max_rolling_quarter(m, wrap = FALSE, na_rm = TRUE)
  expect_equal(idx_nowrap[[1]], 4L)
  expect_true(is.na(idx_nowrap[[2]]))
  
  # For row 3, without wrap, best non-wrap is 1..3 (sum 150) vs 4..6 (sum 102) -> expect 1
  expect_equal(idx_nowrap[[3]], 1L)
  
  idx_wrap <- parallel_which_max_rolling_quarter(m, wrap = TRUE, na_rm = TRUE)
  
  expect_equal(idx_wrap[[1]], 4L)
  expect_true(is.na(idx_wrap[[2]]))
  
  # With wrap, window (6,1,2) = 100+50+50 = 200 -> start=6
  expect_equal(idx_wrap[[3]], 6L)
})

test_that("ties are broken by earliest start index", {
  # Row with two equal max sums: (1,2,3)=6 and (2,3,1)=6 with wrap,
  # expect earliest start (1) if implementation picks first max.
  r <- c(1, 2, 3)
  m <- matrix(r, nrow = 1)
  idx_nowrap <- parallel_which_max_rolling_quarter(m, wrap = FALSE, na_rm = TRUE)
  expect_equal(as.vector(idx_nowrap), 1L)
  
  # With longer row: two equal maxima  (1,2,3)=6 and (2,3,1)=6 via wrap;
  # expect 1 as earliest
  r2 <- c(1, 2, 3, 0, 0, 0)
  m2 <- matrix(r2, nrow = 1)
  idx_wrap <- parallel_which_max_rolling_quarter(m2, wrap = TRUE, na_rm = TRUE)
  expect_equal(as.vector(idx_wrap), 1L)
})

test_that("preserves row names", {
  m <- matrix(1:18, nrow = 3, byrow = TRUE)
  rownames(m) <- c("cell_A", "cell_B", "cell_C")
  idx <- parallel_which_max_rolling_quarter(m, wrap = FALSE, na_rm = FALSE)
  expect_named(idx, rownames(m))
})

test_that("fails with non-matrix input", {
  wrong_input <- c(1, 2, 3)
  expect_error(
    parallel_which_max_rolling_quarter(wrong_input),
    "Must be of type 'matrix'"
  )
})

test_that("fails with non-numeric matrix", {
  wrong_input <- matrix(letters[1:6], nrow = 2)
  expect_error(
    parallel_which_max_rolling_quarter(wrong_input),
    "Must store numerics"
  )
})


test_that("fails with fewer than 3 columns", {
  m <- matrix(as.numeric(1:4), nrow = 2, ncol = 2)
  expect_error(parallel_which_max_rolling_quarter(m))
})


test_that("fails with invalid flags", {
  m <- matrix(1:12, nrow = 3, byrow = TRUE)
  expect_error(parallel_which_max_rolling_quarter(m, wrap = "yes"), regexp = "logical flag")
  expect_error(parallel_which_max_rolling_quarter(m, na_rm = 1L), regexp = "logical flag")
})


test_that("works with a single row and many columns", {
  m <- matrix(c(0,0,0, 10,10,10, 5,5,5), nrow = 1)
  idx <- parallel_which_max_rolling_quarter(m, wrap = FALSE, na_rm = TRUE)
  expect_equal(as.vector(idx), 4L)
})

test_that("NA result when all windows contain NA and na_rm = TRUE", {
  m <- matrix(c(1, NA, 2, NA, 3, NA), nrow = 1)
  idx <- parallel_which_max_rolling_quarter(m, wrap = TRUE, na_rm = TRUE)
  expect_true(is.na(idx[1]))
})
