# fastbioclim

RcppParallel Calculation of Bioclimatic Variables

## Description

Get bioclimatic variables from monthly raster time series of temperature and precipitation. Implementation based on RcppParallel.

## Features

- Fast calculation of bioclimatic variables using RcppParallel
- Optimized C++ backend for performance
- Handles monthly raster time series data efficiently
- Includes 8 bioclimatic variables (bio_01 through bio_08)

## Installation

To install this package, you can use the following R code:

```r
# Install from source if available
# devtools::install_github("alrobles/fastBioClim")
```

## Usage

The package provides functions for calculating various bioclimatic variables:

- `bio_01()` - Annual Mean Temperature
- `bio_02()` - Mean Diurnal Range
- `bio_03()` - Isothermality
- `bio_04()` - Temperature Seasonality
- `bio_05()` - Maximum Temperature of Warmest Month
- `bio_06()` - Minimum Temperature of Coldest Month
- `bio_07()` - Temperature Annual Range
- `bio_08()` - Mean Temperature of Wettest Quarter

## Dependencies

This package requires:
- R (≥ 4.0.0)
- Rcpp
- RcppParallel
- terra
- checkmate

## Contributing

If you find any issues or have suggestions for improvements, please use the issue tracker on the GitHub repository.

## License

GPL (>= 3)

## Author

Angel Robles <a.l.robles.fernandez@gmail.com>

## Bug Reports

Please report bugs at: https://github.com/alrobles/fastBioClim/issues