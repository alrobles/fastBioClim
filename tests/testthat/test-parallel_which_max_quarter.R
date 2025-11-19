library(testthat)
library(checkmate)

test_that("basic behavior without wrap, no NA", {
  # Verify that for this matrix, all three rows return a starting index of 4
  # Matrix has 3 rows and 6 columns
  # Row r1: window means → [1..3]=2, [2..4]=3, [3..5]=4, [4..6]=5 so
  # max at start=4
  # Row r2: window means → [1..3]=4, [2..4]=13/3≈4.3, [3..5]=14/3≈4.6, [4..6]=5
  # so max at start=4
  # Row r3: values = (5,6,7,6,7,8)
  #   windows: [1..3]=6, [2..4]=7, [3..5]=20/3≈6.67, [4..6]=7
  # so max at start=4
  # So expected result: all rows return index 4

  mat <- matrix(
    c(
      1, 2, 3, 2, 3, 4,
      3, 4, 5, 4, 5, 6,
      5, 6, 7, 6, 7, 8
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("r1", "r2", "r3"), paste0("m", 1:6))
  )
  out <- parallel_which_max_quarter(mat, wrap = FALSE, na_rm = FALSE)

  expect_integer(out, len = 3)
  expect_equal(unname(out["r1"]), 4L)
  expect_equal(unname(out["r2"]), 4L)
  expect_equal(unname(out["r3"]), 4L)
})

test_that("basic behavior with wrap", {
  # 1 row x 5 cols; wrap windows start at 1..5
  # Means:
  # s=1: (1,2,10)=13/3; s=2: (2,10,20)=32/3; s=3: (10,20,50)=80/3 (max)
  # s=4: (20,50,1)=71/3; s=5: (50,1,2)=53/3
  mat <- matrix(c(1, 2, 10, 20, 50),
    nrow = 1,
    dimnames = list("only", paste0("m", 1:5))
  )
  out <- parallel_which_max_quarter(mat, wrap = TRUE, na_rm = FALSE)

  expect_integer(out, len = 1)
  expect_equal(unname(out["only"]), 3L) # corrected: max at start=3
})

test_that("na_rm = FALSE invalidates any NA within a window (no wrap)", {
  mat <- matrix(
    c(
      1, NA, 3, 4, NA, # a: every window has an NA
      5, 6, NA, 7, 8
    ), # b: every window has an NA
    nrow = 2, byrow = TRUE,
    dimnames = list(c("a", "b"), paste0("m", 1:5))
  )
  out <- parallel_which_max_quarter(mat, wrap = FALSE, na_rm = FALSE)

  expect_integer(out, len = 2, any.missing = TRUE)
  expect_true(is.na(out["a"]))
  expect_true(is.na(out["b"]))
})

test_that(
  "na_rm = TRUE averages over non-NA; all-NA windows are invalid (no wrap)",
  {
    # r1 windows:
    # [1..3] = (1,NA,3)-> 2;
    # [2..4] = (NA,3,4)-> 3.5;
    # [3..5] = (3,4,NA)-> 3.5 so start 2 or 3
    # r2: all NA -> NA
    mat <- matrix(
      c(
        1, NA, 3, 4, NA,
        NA, NA, NA, NA, NA
      ),
      nrow = 2, byrow = TRUE,
      dimnames = list(c("r1", "r2"), paste0("m", 1:5))
    )
    out <- parallel_which_max_quarter(mat, wrap = FALSE, na_rm = TRUE)

    expect_integer(out, len = 2, any.missing = TRUE)
    expect_true(unname(out["r1"]) %in% c(2L, 3L))
    expect_true(is.na(out["r2"]))
  }
)

test_that("wrap with na_rm = TRUE handles wrapped windows properly", {
  # s=1: (1,2,NA)->1.5;
  # s=2: (2,NA,100)->51 (max);
  # s=3: (NA,100,1)->50.5; s=4: (100,1,2) -> 34.33
  mat <- matrix(c(1, 2, NA, 100),
    nrow = 1,
    dimnames = list("row", paste0("m", 1:4))
  )
  out <- parallel_which_max_quarter(mat, wrap = TRUE, na_rm = TRUE)
  expect_integer(out, len = 1)
  expect_equal(unname(out["row"]), 2L)
})

test_that("all windows invalid -> NA (no wrap)", {
  mat <- matrix(c(NA, NA, NA),
    nrow = 1,
    dimnames = list("r", paste0("m", 1:3))
  )
  out <- parallel_which_max_quarter(mat, wrap = FALSE, na_rm = TRUE)
  expect_true(is.na(out["r"]))
})

test_that("ties: accept either earliest or implementation-defined tie-break", {
  # Two equal-max windows for row 'a'; unique max for 'b'
  mat <- rbind(
    a = c(1, 2, 3, 1), # [1..3] mean=2; [2..4]=(2,3,1) mean=2 -> accept 1 or 2
    b = c(3, 2, 1, 1) # [1..3] mean=2; [2..4]=(2,1,1) mean=4/3 -> max at 1
  )
  out <- parallel_which_max_quarter(mat, wrap = FALSE, na_rm = FALSE)
  expect_true(unname(out["a"]) %in% c(1L, 2L))
  expect_equal(unname(out["b"]), 1L)
})

test_that("input validation: matrix type, numeric mode, size", {
  expect_error(parallel_which_max_quarter(1:5), "Must be of type 'matrix'")
  expect_error(
    parallel_which_max_quarter(matrix(letters[1:6], nrow = 2)),
    "Must store numerics"
  )
  expect_error(
    parallel_which_max_quarter(matrix(1:4, nrow = 2)),
    "Must have at least 3 cols"
  )
})

test_that("row names are preserved", {
  mat <- matrix(1:12,
    nrow = 3, byrow = TRUE,
    dimnames = list(c("r1", "r2", "r3"), paste0("m", 1:4))
  )
  out <- parallel_which_max_quarter(mat)
  expect_identical(names(out), rownames(mat))
})
