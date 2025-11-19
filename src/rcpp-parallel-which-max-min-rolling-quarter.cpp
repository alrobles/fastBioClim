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
      if (ncol < 3) {
        out(i, 0) = NA_INTEGER;
        continue;
      }
      
      const std::size_t nwin = wrap ? ncol : (ncol - 2);
      bool any_valid = false;
      double best_score = -std::numeric_limits<double>::infinity();
      std::size_t best_start = 0; // 0-based
      
      for (std::size_t j = 0; j < nwin; ++j) {
        // 3-month window starting at j
        std::size_t j0 = j;
        std::size_t j1 = wrap ? (j + 1) % ncol : (j + 1);
        std::size_t j2 = wrap ? (j + 2) % ncol : (j + 2);
        if (!wrap && j2 >= ncol) break; // guard for no-wrap
        
        const double a = m(i, j0);
        const double b = m(i, j1);
        const double c = m(i, j2);
        
        double score = NA_REAL;
        
        if (!na_rm) {
          // Window invalid if any NA present
          if (is_na_double(a) || is_na_double(b) || is_na_double(c)) {
            score = NA_REAL;
          } else {
            // Fixed window size -> mean vs sum same ordering; use mean
            score = (a + b + c) / 3.0;
          }
        } else {
          // na_rm = TRUE: compute mean over non-NA; invalid if all NA
          double sum = 0.0;
          int cnt = 0;
          if (!is_na_double(a)) { sum += a; ++cnt; }
          if (!is_na_double(b)) { sum += b; ++cnt; }
          if (!is_na_double(c)) { sum += c; ++cnt; }
          if (cnt == 0) {
            score = NA_REAL; // all NA -> invalid window
          } else {
            score = sum / static_cast<double>(cnt);
          }
        }
        
        // Update best; earliest index wins ties (strictly greater update)
        if (!std::isnan(score)) {
          if (!any_valid || (score > best_score)) {
            any_valid = true;
            best_score = score;
            best_start = j;
          }
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
      if (ncol < 3) {
        out(i, 0) = NA_INTEGER;
        continue;
      }
      
      const std::size_t nwin = wrap ? ncol : (ncol - 2);
      bool any_valid = false;
      double best_score = std::numeric_limits<double>::infinity();
      std::size_t best_start = 0; // 0-based
      
      for (std::size_t j = 0; j < nwin; ++j) {
        std::size_t j0 = j;
        std::size_t j1 = wrap ? (j + 1) % ncol : (j + 1);
        std::size_t j2 = wrap ? (j + 2) % ncol : (j + 2);
        if (!wrap && j2 >= ncol) break;
        
        const double a = m(i, j0);
        const double b = m(i, j1);
        const double c = m(i, j2);
        
        double score = NA_REAL;
        
        if (!na_rm) {
          // Window invalid if any NA present
          if (is_na_double(a) || is_na_double(b) || is_na_double(c)) {
            score = NA_REAL;
          } else {
            score = (a + b + c) / 3.0;
          }
        } else {
          // na_rm = TRUE: compute mean over non-NA; invalid if all NA
          double sum = 0.0;
          int cnt = 0;
          if (!is_na_double(a)) { sum += a; ++cnt; }
          if (!is_na_double(b)) { sum += b; ++cnt; }
          if (!is_na_double(c)) { sum += c; ++cnt; }
          if (cnt == 0) {
            score = NA_REAL; // all NA -> invalid window
          } else {
            score = sum / static_cast<double>(cnt);
          }
        }
        
        // Update best; earliest index wins ties (strictly smaller update)
        if (!std::isnan(score)) {
          if (!any_valid || (score < best_score)) {
            any_valid = true;
            best_score = score;
            best_start = j;
          }
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
