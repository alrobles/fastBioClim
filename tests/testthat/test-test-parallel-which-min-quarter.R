library(testthat)
library(checkmate)

test_that("basic behavior without wrap, no NA (min over 3-col sums)", {
  # 3 rows x 6 cols
  # r1: c(1,2,3, 2,3,4) -> windows sums:
  # [1..3]=6, [2..4]=9, [3..5]=12, [4..6]=15 -> min at start=1
  # r2: c(2,3,4, 3,4,5) -> windows sums:
  # [1..3]=9, [2..4]=12, [3..5]=15, [4..6]=18 -> min at start=1
  # r3: c(3,4,5, 4,5,6) -> windows sums:
  # [1..3]=12, [2..4]=15, [3..5]=18, [4..6]=21 -> min at start=1
  mat <- matrix(
    c(1, 2, 3, 2, 3, 4, 3, 4, 5, 4, 5, 6, 5, 6, 7, 6, 7, 8),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("r1", "r2", "r3"), paste0("m", 1:6))
  )
  out <- parallel_which_min_quarter(mat, wrap = FALSE, na_rm = FALSE)

  expect_integer(out, len = 3)
  expect_equal(unname(out["r1"]), 1L)
  expect_equal(unname(out["r2"]), 1L)
  expect_equal(unname(out["r3"]), 1L)
})

test_that("basic behavior with wrap (min over 3-col sums)", {
  # 1 row x 5 cols; wrap windows start at 1..5
  # Sums:
  # s=1: (1+2+10) is 13
  # s=2: (2+10+20) is 32
  # s=3: (10+20+50) is 80
  # s=4: (20+50+1) is 71
  # s=5: (50+1+2) is 53
  # -> min at start = 1
  mat <- matrix(c(1, 2, 10, 20, 50),
    nrow = 1,
    dimnames = list("only", paste0("m", 1:5))
  )
  out <- parallel_which_min_quarter(mat, wrap = TRUE, na_rm = FALSE)

  expect_integer(out, len = 1)
  expect_equal(unname(out["only"]), 1L)
})

test_that("na_rm = FALSE invalidates any NA within a window (no wrap)", {
  # Row a: every window has NA -> NA
  # Row b: every window has NA -> NA
  mat <- matrix(
    c(
      1, NA, 3, 4, NA, # a
      5, 6, NA, 7, 8
    ), # b
    nrow = 2, byrow = TRUE,
    dimnames = list(c("a", "b"), paste0("m", 1:5))
  )
  out <- parallel_which_min_quarter(mat, wrap = FALSE, na_rm = FALSE)

  expect_integer(out, len = 2, any.missing = TRUE)
  expect_true(is.na(out["a"]))
  expect_true(is.na(out["b"]))
})

test_that(
  "na_rm = TRUE sums over non-NA;
  all-NA windows invalid; choose minimum (no wrap)",
  {
    # r1 windows:
    # [1..3] = (1, NA, 3) -> sum=4 over 2 non-NA
    # [2..4] = (NA, 3, 4) -> sum=7
    # [3..5] = (3, 4, NA) -> sum=7
    # -> minimum sum = 4 at start = 1
    #
    # r2: all NA -> NA
    mat <- matrix(
      c(
        1, NA, 3, 4, NA,
        NA, NA, NA, NA, NA
      ),
      nrow = 2, byrow = TRUE,
      dimnames = list(c("r1", "r2"), paste0("m", 1:5))
    )
    out <- parallel_which_min_quarter(mat, wrap = FALSE, na_rm = TRUE)

    expect_integer(out, len = 2, any.missing = TRUE)
    expect_equal(unname(out["r1"]), 1L)
    expect_true(is.na(out["r2"]))
  }
)

test_that("wrap with na_rm = TRUE handles wrapped windows and NAs", {
  # Single row, 4 cols
  # Use values where the smallest sum window is wrapped at start=1:
  # s=1: (1,2,NA) -> sum = 3
  # s=2: (2,NA,100) -> sum = 102
  # s=3: (NA,100,1) -> sum = 101
  # s=4: (100,1,2) -> sum = 103
  mat <- matrix(c(1, 2, NA, 100),
    nrow = 1,
    dimnames = list("row", paste0("m", 1:4))
  )
  out <- parallel_which_min_quarter(mat, wrap = TRUE, na_rm = TRUE)

  expect_integer(out, len = 1)
  expect_equal(unname(out["row"]), 1L)
})

test_that("all windows invalid -> NA (no wrap)", {
  mat <- matrix(c(NA, NA, NA),
    nrow = 1,
    dimnames = list("r", paste0("m", 1:3))
  )
  out <- parallel_which_min_quarter(mat, wrap = FALSE, na_rm = TRUE)
  expect_true(is.na(out["r"]))
})

test_that("ties: accept either valid start when sums tie (no wrap)", {
  # Row a: two equal-min windows
  # a is c(1, 2, 3, 1)
  # windows: [1..3]=1+2+3=6, [2..4]=2+3+1=6 -> 1 or 2
  # Row b: unique min at [2..4]: 2+1+1=4 vs [1..3] 3+2+1=6
  mat <- rbind(
    a = c(1, 2, 3, 1),
    b = c(3, 2, 1, 1)
  )
  out <- parallel_which_min_quarter(mat, wrap = FALSE, na_rm = FALSE)

  expect_true(unname(out["a"]) %in% c(1L, 2L))
  expect_equal(unname(out["b"]), 2L)
})

test_that("input validation: matrix type, numeric mode, size", {
  # non-matrix
  expect_error(parallel_which_min_quarter(1:5), "Must be of type 'matrix'")
  # non-numeric matrix
  expect_error(
    parallel_which_min_quarter(matrix(letters[1:6], nrow = 2)),
    "Must store numerics"
  )
  # fewer than 3 columns (checkmate message)
  expect_error(
    parallel_which_min_quarter(matrix(1:4, nrow = 2)),
    "Must have at least 3 cols"
  )
})

test_that("row names are preserved", {
  mat <- matrix(1:12,
    nrow = 3, byrow = TRUE,
    dimnames = list(c("r1", "r2", "r3"), paste0("m", 1:4))
  )
  out <- parallel_which_min_quarter(mat)
  expect_identical(names(out), rownames(mat))
})
