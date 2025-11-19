#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <numeric>
#include <cmath>              // for std::sqrt
using namespace Rcpp;

#include <RcppParallel.h>
using namespace RcppParallel;

// generic function for variance: returns M2 (sum of squared deviations)
// If any NA/NaN encountered, returns NaN (so callers can map to NA_real_).
template <typename InputIterator>
inline double variance(InputIterator begin, InputIterator end) {
  double mean = 0.0;
  double M2   = 0.0;
  double count = 0.0;
  
  InputIterator it = begin;
  while (it != end) {
    double x = *it++;
    // If NA/NaN, propagate as NaN (base R var with na.rm = FALSE -> NA)
    if (Rcpp::NumericVector::is_na(x) || std::isnan(x)) {
      return std::numeric_limits<double>::quiet_NaN();
    }
    count += 1.0;
    double delta  = x - mean;
    mean += delta / count;
    double delta2 = x - mean;
    M2 += delta * delta2;
  }
  return M2;
}

struct VarParallel : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;
  
  // output matrix to write to
  RMatrix<double> rmat;
  
  VarParallel(const NumericMatrix mat_1, NumericMatrix rmat)
    : mat_1(mat_1), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      RMatrix<double>::Row v1 = mat_1.row(i);
      
      // Handle length < 2 like base R: NA
      if (v1.size() < 2) {
        rmat(i, 0) = NA_REAL;
        continue;
      }
      
      double M2 = variance(v1.begin(), v1.end());
      
      // If any NA encountered, M2 is NaN -> return NA_real_
      if (std::isnan(M2)) {
        rmat(i, 0) = NA_REAL;
      } else {
        rmat(i, 0) = M2 / static_cast<double>(v1.size() - 1);
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_variance(NumericMatrix mat_1) {
  // allocate the matrix we will return
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  VarParallel varparallel(mat_1, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), varparallel);
  
  return rmat;
}

struct SDParallel : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;
  
  // output matrix to write to
  RMatrix<double> rmat;
  
  SDParallel(const NumericMatrix mat_1, NumericMatrix rmat)
    : mat_1(mat_1), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      RMatrix<double>::Row v1 = mat_1.row(i);
      
      // Handle length < 2 like base R: NA
      if (v1.size() < 2) {
        rmat(i, 0) = NA_REAL;
        continue;
      }
      
      double M2 = variance(v1.begin(), v1.end());
      
      if (std::isnan(M2)) {
        rmat(i, 0) = NA_REAL;
      } else {
        rmat(i, 0) = std::sqrt(M2 / static_cast<double>(v1.size() - 1));
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_sd(NumericMatrix mat_1) {
  // allocate the matrix we will return
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  SDParallel sdparallel(mat_1, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), sdparallel);
  
  return rmat;
}
