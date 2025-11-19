// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cmath>
using namespace Rcpp;
using namespace RcppParallel;

inline bool is_na_double(double x) {
  return Rcpp::NumericVector::is_na(x) || std::isnan(x);
}

struct AverageQuarterWorker : public Worker {
  const RMatrix<double> ix;   // nrow x 1, 1-based start indices
  const RMatrix<double> m;    // data matrix
  RMatrix<double> out;        // nrow x 1, average
  const bool wrap;
  const bool na_rm;
  
  AverageQuarterWorker(const NumericMatrix& ixQuarter,
                       const NumericMatrix& mat,
                       NumericMatrix& out_,
                       bool wrap_,
                       bool na_rm_)
    : ix(ixQuarter), m(mat), out(out_), wrap(wrap_), na_rm(na_rm_) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    const std::size_t ncol = m.ncol();
    
    for (std::size_t i = begin; i < end; ++i) {
      const double idx_d = ix(i, 0);
      
      if (is_na_double(idx_d)) {
        out(i, 0) = NA_REAL;
        continue;
      }
      
      long start1 = static_cast<long>(idx_d);
      if (start1 < 1 || start1 > static_cast<long>(ncol)) {
        out(i, 0) = NA_REAL;
        continue;
      }
      
      std::size_t j0 = static_cast<std::size_t>(start1 - 1);
      std::size_t j1 = wrap ? (j0 + 1) % ncol : (j0 + 1);
      std::size_t j2 = wrap ? (j0 + 2) % ncol : (j0 + 2);
      
      if (!wrap && (j2 >= ncol)) {
        out(i, 0) = NA_REAL;
        continue;
      }
      
      const double a = m(i, j0);
      const double b = m(i, j1);
      const double c = m(i, j2);
      
      if (!na_rm) {
        // Propagate NA if any NA present in the window
        if (is_na_double(a) || is_na_double(b) || is_na_double(c)) {
          out(i, 0) = NA_REAL;
        } else {
          out(i, 0) = (a + b + c) / 3.0;
        }
      } else {
        // Average over non-NA values; NA if all 3 are NA
        double sum = 0.0;
        int cnt = 0;
        if (!is_na_double(a)) { sum += a; ++cnt; }
        if (!is_na_double(b)) { sum += b; ++cnt; }
        if (!is_na_double(c)) { sum += c; ++cnt; }
        out(i, 0) = (cnt == 0) ? NA_REAL : (sum / static_cast<double>(cnt));
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_average_quarter(const NumericMatrix& ixQuarter,
                                            const NumericMatrix& mat,
                                            bool wrap = false,
                                            bool na_rm = false) {
  if (mat.ncol() < 3)
    stop("Matrix must have at least 3 columns.");
  if (ixQuarter.ncol() != 1)
    stop("ixQuarter must have exactly one column of 1-based start indices.");
  if (ixQuarter.nrow() != mat.nrow())
    stop("ixQuarter must have the same number of rows as 'mat'.");
  
  NumericMatrix out(mat.nrow(), 1);
  AverageQuarterWorker worker(ixQuarter, mat, out, wrap, na_rm);
  parallelFor(0, mat.nrow(), worker);
  return out;
}