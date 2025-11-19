// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cmath>
#include <limits>

using namespace Rcpp;
using namespace RcppParallel;

inline bool is_na_double(double x) {
  return Rcpp::NumericVector::is_na(x) || std::isnan(x);
}

struct RollingMaxQuarterIdx : public Worker {
  const RMatrix<double> m;
  RMatrix<int> out;
  const bool wrap;
  const bool na_rm;
  
  RollingMaxQuarterIdx(const NumericMatrix& mat,
                 IntegerMatrix& out_,
                 bool wrap_,
                 bool na_rm_)
    : m(mat), out(out_), wrap(wrap_), na_rm(na_rm_) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    const std::size_t ncol = m.ncol();
    for (std::size_t i = begin; i < end; ++i) {
      auto row = m.row(i);
      
      if (ncol < 3) { out(i, 0) = NA_INTEGER; continue; }
      
      // Strict NA policy: if any NA in the row and na_rm == false => NA
      if (!na_rm) {
        bool any_na = false;
        for (std::size_t j = 0; j < ncol; ++j) {
          if (is_na_double(row[j])) { any_na = true; break; }
        }
        if (any_na) { out(i, 0) = NA_INTEGER; continue; }
      }
      
      const std::size_t nwin = wrap ? ncol : (ncol - 2);
      double best = -std::numeric_limits<double>::infinity();
      std::size_t best_start = 0;
      bool any_valid = false;
      
      for (std::size_t j = 0; j < nwin; ++j) {
        // 3-month window starting at j
        std::size_t j0 = j;
        std::size_t j1 = wrap ? (j + 1) % ncol : (j + 1);
        std::size_t j2 = wrap ? (j + 2) % ncol : (j + 2);
        if (!wrap && j2 >= ncol) break;
        
        const double a = row[j0], b = row[j1], c = row[j2];
        if (is_na_double(a) || is_na_double(b) || is_na_double(c)) {
          if (na_rm) continue;       // skip this window
          // (na_rm == false handled above)
        }
        const double s = a + b + c;
        if (!any_valid || s > best) {
          best = s; best_start = j; any_valid = true;
        }
      }
      
      out(i, 0) = any_valid ? static_cast<int>(best_start + 1) : NA_INTEGER; // 1-based
    }
  }
};

struct RollingMinQuarterIdx : public Worker {
  const RMatrix<double> m;
  RMatrix<int> out;
  const bool wrap;
  const bool na_rm;
  
  RollingMinQuarterIdx(const NumericMatrix& mat,
                 IntegerMatrix& out_,
                 bool wrap_,
                 bool na_rm_)
    : m(mat), out(out_), wrap(wrap_), na_rm(na_rm_) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    const std::size_t ncol = m.ncol();
    for (std::size_t i = begin; i < end; ++i) {
      auto row = m.row(i);
      
      if (ncol < 3) { out(i, 0) = NA_INTEGER; continue; }
      
      if (!na_rm) {
        bool any_na = false;
        for (std::size_t j = 0; j < ncol; ++j) {
          if (is_na_double(row[j])) { any_na = true; break; }
        }
        if (any_na) { out(i, 0) = NA_INTEGER; continue; }
      }
      
      const std::size_t nwin = wrap ? ncol : (ncol - 2);
      double best =  std::numeric_limits<double>::infinity();
      std::size_t best_start = 0;
      bool any_valid = false;
      
      for (std::size_t j = 0; j < nwin; ++j) {
        std::size_t j0 = j;
        std::size_t j1 = wrap ? (j + 1) % ncol : (j + 1);
        std::size_t j2 = wrap ? (j + 2) % ncol : (j + 2);
        if (!wrap && j2 >= ncol) break;
        
        const double a = row[j0], b = row[j1], c = row[j2];
        if (is_na_double(a) || is_na_double(b) || is_na_double(c)) {
          if (na_rm) continue;
        }
        const double s = a + b + c;
        if (!any_valid || s < best) {
          best = s; best_start = j; any_valid = true;
        }
      }
      
      out(i, 0) = any_valid ? static_cast<int>(best_start + 1) : NA_INTEGER; // 1-based
    }
  }
};

// [[Rcpp::export]]
IntegerMatrix rcpp_parallel_which_max_rolling_quarter(const NumericMatrix& mat,
                                               bool wrap = false,
                                               bool na_rm = false) {
  if (mat.ncol() < 3) stop("Matrix must have at least 3 columns.");
  IntegerMatrix out(mat.nrow(), 1);
  RollingMaxQuarterIdx worker(mat, out, wrap, na_rm);
  parallelFor(0, mat.nrow(), worker);
  return out;
}

// [[Rcpp::export]]
IntegerMatrix rcpp_parallel_which_min_rolling_quarter(const NumericMatrix& mat,
                                               bool wrap = false,
                                               bool na_rm = false) {
  if (mat.ncol() < 3) stop("Matrix must have at least 3 columns.");
  IntegerMatrix out(mat.nrow(), 1);
  RollingMinQuarterIdx worker(mat, out, wrap, na_rm);
  parallelFor(0, mat.nrow(), worker);
  return out;
}