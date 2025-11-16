library(testthat)
library(terra)

test_that("mock_tasmax returns a SpatRaster with 12 layers", {
  # Call the function
  tasmax <- mock_tasmax()

  # Check class
  expect_s4_class(tasmax, "SpatRaster")

  # Check number of layers
  expect_equal(nlyr(tasmax), 12)

  # Check layer names follow expected pattern
  expected_names <- sprintf("tasmax_%02d", 1:12)
  expect_equal(names(tasmax), expected_names)

  # Check dimensions
  expect_equal(dim(tasmax)[1:2], c(20, 20)) # 20 rows, 20 cols

  # Check CRS is WGS84
  expect_true(grepl("4326", crs(tasmax)))
})
