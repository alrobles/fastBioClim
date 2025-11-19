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

struct MinQuarter : public Worker {
  const RMatrix<double> m1, m2, m3;
  RMatrix<int> out;
  const bool na_rm;
  
  MinQuarter(const NumericMatrix& a,
                const NumericMatrix& b,
                const NumericMatrix& c,
                IntegerMatrix& out_,
                bool na_rm_)
    : m1(a), m2(b), m3(c), out(out_), na_rm(na_rm_) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    const std::size_t ncol = m1.ncol();
    
    for (std::size_t i = begin; i < end; ++i) {
      auto r1 = m1.row(i);
      auto r2 = m2.row(i);
      auto r3 = m3.row(i);
      
      if (!na_rm) {
        bool any_na = false;
        for (std::size_t j = 0; j < ncol; ++j) {
          if (is_na_double(r1[j]) || is_na_double(r2[j]) || is_na_double(r3[j])) {
            any_na = true; break;
          }
        }
        if (any_na) { out(i, 0) = NA_INTEGER; continue; }
      }
      
      double best = std::numeric_limits<double>::infinity();
      std::size_t best_j = 0;
      bool any_valid = false;
      
      for (std::size_t j = 0; j < ncol; ++j) {
        const double a = r1[j], b = r2[j], c = r3[j];
        if (is_na_double(a) || is_na_double(b) || is_na_double(c)) {
          if (na_rm) continue; // skip this position
          // (na_rm=false handled above)
        }
        const double s = a + b + c;
        if (!any_valid || s < best) {
          best = s;
          best_j = j;
          any_valid = true;
        }
      }
      
      out(i, 0) = any_valid ? static_cast<int>(best_j + 1) : NA_INTEGER; // 1-based
    }
  }
};

struct MaxQuarter : public Worker {
  const RMatrix<double> m1, m2, m3;
  RMatrix<int> out;
  const bool na_rm;
  
  MaxQuarter(const NumericMatrix& a,
                const NumericMatrix& b,
                const NumericMatrix& c,
                IntegerMatrix& out_,
                bool na_rm_)
    : m1(a), m2(b), m3(c), out(out_), na_rm(na_rm_) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    const std::size_t ncol = m1.ncol();
    
    for (std::size_t i = begin; i < end; ++i) {
      auto r1 = m1.row(i);
      auto r2 = m2.row(i);
      auto r3 = m3.row(i);
      
      if (!na_rm) {
        bool any_na = false;
        for (std::size_t j = 0; j < ncol; ++j) {
          if (is_na_double(r1[j]) || is_na_double(r2[j]) || is_na_double(r3[j])) {
            any_na = true; break;
          }
        }
        if (any_na) { out(i, 0) = NA_INTEGER; continue; }
      }
      
      double best = -std::numeric_limits<double>::infinity();
      std::size_t best_j = 0;
      bool any_valid = false;
      
      for (std::size_t j = 0; j < ncol; ++j) {
        const double a = r1[j], b = r2[j], c = r3[j];
        if (is_na_double(a) || is_na_double(b) || is_na_double(c)) {
          if (na_rm) continue; // skip this position
          // (na_rm=false handled above)
        }
        const double s = a + b + c;
        if (!any_valid || s > best) {
          best = s;
          best_j = j;
          any_valid = true;
        }
      }
      
      out(i, 0) = any_valid ? static_cast<int>(best_j + 1) : NA_INTEGER; // 1-based
    }
  }
};

// [[Rcpp::export]]
IntegerMatrix rcpp_parallel_which_min_quarter_idx(const NumericMatrix& mat_1,
                                                  const NumericMatrix& mat_2,
                                                  const NumericMatrix& mat_3,
                                                  bool na_rm = false) {
  if (mat_1.nrow() != mat_2.nrow() || mat_1.nrow() != mat_3.nrow())
    stop("All matrices must have the same number of rows");
  if (mat_1.ncol() != mat_2.ncol() || mat_1.ncol() != mat_3.ncol())
    stop("All matrices must have the same number of columns");
  
  IntegerMatrix out(mat_1.nrow(), 1);
  MinQuarter worker(mat_1, mat_2, mat_3, out, na_rm);
  parallelFor(0, mat_1.nrow(), worker);
  return out;
}

// [[Rcpp::export]]
IntegerMatrix rcpp_parallel_which_max_quarter_idx(const NumericMatrix& mat_1,
                                                  const NumericMatrix& mat_2,
                                                  const NumericMatrix& mat_3,
                                                  bool na_rm = false) {
  if (mat_1.nrow() != mat_2.nrow() || mat_1.nrow() != mat_3.nrow())
    stop("All matrices must have the same number of rows");
  if (mat_1.ncol() != mat_2.ncol() || mat_1.ncol() != mat_3.ncol())
    stop("All matrices must have the same number of columns");
  
  IntegerMatrix out(mat_1.nrow(), 1);
  MaxQuarter worker(mat_1, mat_2, mat_3, out, na_rm);
  parallelFor(0, mat_1.nrow(), worker);
  return out;
}