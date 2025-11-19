#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

inline bool is_na_double(double x) {
  return Rcpp::NumericVector::is_na(x) || std::isnan(x);
}

struct MaxMonth : public Worker {
  const RMatrix<double> mat_1;
  RMatrix<double> rmat;
  
  MaxMonth(const NumericMatrix& mat_1_, NumericMatrix& rmat_)
    : mat_1(mat_1_), rmat(rmat_) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; ++i) {
      RMatrix<double>::Row row = mat_1.row(i);
      
      // Empty row -> NA
      if (row.size() == 0) {
        rmat(i, 0) = NA_REAL;
        continue;
      }
      
      // If any NA present -> NA (na.rm = FALSE policy)
      bool any_na = false;
      for (auto it = row.begin(); it != row.end(); ++it) {
        if (is_na_double(*it)) { any_na = true; break; }
      }
      if (any_na) {
        rmat(i, 0) = NA_REAL;
        continue;
      }
      
      // Find value of the first maximum
      auto it_max = std::max_element(row.begin(), row.end());
      rmat(i, 0) = *it_max; // write the value
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_which_max_row(NumericMatrix mat_1) {
  NumericMatrix rmat(mat_1.nrow(), 1);
  MaxMonth maxmonth(mat_1, rmat);
  parallelFor(0, mat_1.nrow(), maxmonth);
  return rmat;
}

struct MinMonth : public Worker {
  const RMatrix<double> mat_1;
  RMatrix<double> rmat;
  
  MinMonth(const NumericMatrix& mat_1_, NumericMatrix& rmat_)
    : mat_1(mat_1_), rmat(rmat_) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; ++i) {
      RMatrix<double>::Row row = mat_1.row(i);
      
      // Empty row -> NA
      if (row.size() == 0) {
        rmat(i, 0) = NA_REAL;
        continue;
      }
      
      // If any NA present -> NA (na.rm = FALSE policy)
      bool any_na = false;
      for (auto it = row.begin(); it != row.end(); ++it) {
        if (is_na_double(*it)) { any_na = true; break; }
      }
      if (any_na) {
        rmat(i, 0) = NA_REAL;
        continue;
      }
      
      // Find value of the first minimum
      auto it_min = std::min_element(row.begin(), row.end());
      rmat(i, 0) = *it_min; // write the value
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_which_min_row(NumericMatrix mat_1) {
  NumericMatrix rmat(mat_1.nrow(), 1);
  MinMonth minmonth(mat_1, rmat);
  parallelFor(0, mat_1.nrow(), minmonth);
  return rmat;
}