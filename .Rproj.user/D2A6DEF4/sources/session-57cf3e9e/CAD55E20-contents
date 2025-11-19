library(testthat)
library(terra)

test_that("mock_pr returns a SpatRaster with 12 layers", {
  # Call the function
  pr <- mock_pr()

  # Check class
  expect_s4_class(pr, "SpatRaster")

  # Check number of layers
  expect_equal(nlyr(pr), 12)

  # Check layer names follow expected pattern
  expected_names <- sprintf("pr_%02d", 1:12)
  expect_equal(names(pr), expected_names)

  # Check dimensions
  expect_equal(dim(pr)[1:2], c(20, 20)) # 20 rows, 20 cols

  # Check CRS is WGS84
  expect_true(grepl("4326", crs(pr)))
})
