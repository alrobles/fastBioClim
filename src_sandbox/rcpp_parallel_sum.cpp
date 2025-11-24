// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <numeric>
#include <cmath>

using namespace Rcpp;
using namespace RcppParallel;

// Helper to check NA or NaN
inline bool is_na_double(double x) {
  return Rcpp::NumericVector::is_na(x) || std::isnan(x);
}

struct SumWorker : public Worker {
  const RMatrix<double> mat;  // input matrix
  RMatrix<double> out;        // output matrix
  const bool na_rm;           // remove NA flag
  
  SumWorker(const NumericMatrix& mat_, NumericMatrix& out_, bool na_rm_)
    : mat(mat_), out(out_), na_rm(na_rm_) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; ++i) {
      RMatrix<double>::Row row = mat.row(i);
      
      double sum = 0.0;
      bool any_na = false;
      int count = 0;
      
      for (auto it = row.begin(); it != row.end(); ++it) {
        double val = *it;
        if (is_na_double(val)) {
          if (!na_rm) {
            any_na = true;
            break; // propagate NA if na_rm = FALSE
          }
        } else {
          sum += val;
          ++count;
        }
      }
      
      if (!na_rm && any_na) {
        out(i, 0) = NA_REAL;
      } else {
        // If na_rm = TRUE and all values were NA, return NA
        out(i, 0) = (count == 0) ? NA_REAL : sum;
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_sum(NumericMatrix mat, bool na_rm = false) {
  NumericMatrix out(mat.nrow(), 1);
  SumWorker worker(mat, out, na_rm);
  parallelFor(0, mat.nrow(), worker);
  return out;
}
