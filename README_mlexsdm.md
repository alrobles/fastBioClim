# mlexsdm

Machine Learning for Ecological Species Distribution Modeling (mlexsdm) is an advanced R package designed to facilitate the development, evaluation, and application of species distribution models using cutting-edge machine learning techniques. This package leverages high-performance computing through Rcpp and RcppParallel to efficiently process large ecological datasets.

## Description

mlexsdm provides a comprehensive framework for implementing various machine learning algorithms specifically tailored for species distribution modeling. The package integrates bioclimatic variable calculations, multiple modeling approaches, preprocessing tools, and evaluation metrics to support ecological research and conservation planning. With optimized C++ implementations, mlexsdm delivers fast processing of environmental raster data for species distribution modeling applications.

## Features

- High-performance bioclimatic variable calculations using RcppParallel
- Multiple machine learning algorithms optimized for species distribution modeling
- Data preprocessing and feature engineering tools
- Cross-validation and model evaluation utilities
- Spatial prediction and mapping capabilities
- Ensemble modeling approaches
- Model interpretation and visualization functions
- Efficient processing of large raster datasets

## Installation

To install this package, you can use the following R code:

```r
# Install from source if available
# devtools::install_github("username/mlexsdm")

# Or install from local source
# devtools::install()
```

## Usage

The package provides functions for various aspects of species distribution modeling:

- Bioclimatic variable calculations (bio_01 to bio_19)
- Data preparation and preprocessing
- Model training and validation
- Spatial prediction and uncertainty quantification
- Model comparison and ensemble methods
- Visualization and interpretation tools

Example usage:
```r
library(mlexsdm)

# Calculate bioclimatic variables from monthly climate data
# bio1 <- bio_01(tmin, tmax, prec)  # Annual Mean Temperature
# Additional bioclimatic variables available through bio_02 to bio_19
```

## Dependencies

This package requires:
- R (≥ 4.0.0)
- Rcpp
- RcppParallel
- terra
- checkmate
- Various machine learning libraries
- Spatial analysis packages
- Data manipulation libraries

## Contributing

If you find any issues or have suggestions for improvements, please use the issue tracker on the GitHub repository.

## License

GPL (>= 3)

## Author

mlexsdm Development Team

## Bug Reports

Please report bugs at: https://github.com/username/mlexsdm/issues