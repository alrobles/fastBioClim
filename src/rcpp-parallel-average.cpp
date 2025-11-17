// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <algorithm>
#include <RcppParallel.h>
#include <cmath>
#include <limits>
#include <stdexcept>

// CRAN-compliant OpenMP configuration
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace RcppParallel;

// Helper function to check for user interrupts with thread safety
inline void check_user_interrupt() {
  if (Rf_interruptsSuspended() == 0) {
    Rcpp::checkUserInterrupt();
  }
}

struct MeanParallel : public Worker {
  const RMatrix<double> mat_1;
  RMatrix<double> rmat;
  const std::size_t n_cols;
  const bool na_rm;
  IntegerVector na_counts;  // Track NA counts per row
  
  MeanParallel(const NumericMatrix& mat_1, NumericMatrix& rmat, bool na_rm)
    : mat_1(mat_1), rmat(rmat), n_cols(mat_1.ncol()), na_rm(na_rm),
      na_counts(rmat.nrow(), 0) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    // Check for user interrupts every 1000 rows (thread-safe)
    const std::size_t interrupt_interval = 1000;
    
    for (std::size_t i = begin; i < end; i++) {
      if ((i - begin) % interrupt_interval == 0) {
        check_user_interrupt();
      }
      
      RMatrix<double>::Row row = mat_1.row(i);
      double sum = 0.0;
      std::size_t valid_count = 0;
      
      for (std::size_t j = 0; j < n_cols; j++) {
        double val = row[j];
        if (!ISNAN(val)) {
          sum += val;
          valid_count++;
        }
      }
      
      if (na_rm && valid_count > 0) {
        rmat(i, 0) = sum / valid_count;
        na_counts[i] = n_cols - valid_count;
      } else if (!na_rm && valid_count == n_cols) {
        rmat(i, 0) = sum / n_cols;
      } else {
        rmat(i, 0) = NA_REAL;
      }
    }
  }
};

// [[Rcpp::export]]
List rcpp_parallel_average(NumericMatrix mat_1, bool na_rm = true) {
  // Input validation
  if (mat_1.ncol() == 0) {
    stop("Input matrix must have at least one column");
  }
  
  if (mat_1.nrow() == 0) {
    return List::create(_["result"] = NumericMatrix(0, 1),
                        _["na_counts"] = IntegerVector(0));
  }
  
  // Create output matrix
  NumericMatrix rmat(mat_1.nrow(), 1);
  IntegerVector na_counts(mat_1.nrow());
  
  // Handle single-row case efficiently (avoid parallel overhead)
  if (mat_1.nrow() == 1) {
    RMatrix<double>::Row row = RMatrix<double>(mat_1).row(0);
    double sum = 0.0;
    std::size_t valid_count = 0;
    
    for (std::size_t j = 0; j < mat_1.ncol(); j++) {
      double val = row[j];
      if (!ISNAN(val)) {
        sum += val;
        valid_count++;
      }
    }
    
    if (na_rm && valid_count > 0) {
      rmat(0, 0) = sum / valid_count;
      na_counts[0] = mat_1.ncol() - valid_count;
    } else if (!na_rm && valid_count == mat_1.ncol()) {
      rmat(0, 0) = sum / mat_1.ncol();
    } else {
      rmat(0, 0) = NA_REAL;
      na_counts[0] = mat_1.ncol();
    }
    
    return List::create(_["result"] = rmat,
                        _["na_counts"] = na_counts);
  }
  
  // Parallel processing with interrupt protection
  try {
    MeanParallel meanparallel(mat_1, rmat, na_rm);
    
    // Configure parallel execution with thread safety
#ifdef _OPENMP
    int max_threads = std::max(1, std::min(omp_get_max_threads(), 8)); // Cap at 8 threads
    omp_set_num_threads(max_threads);
#endif
    
    parallelFor(0, mat_1.nrow(), meanparallel);
    
    return List::create(_["result"] = rmat,
                        _["na_counts"] = meanparallel.na_counts);
  } catch (const std::exception& e) {
    Rcpp::stop("Parallel computation failed: %s", e.what());
  } catch (...) {
    Rcpp::stop("Unknown error in parallel computation");
  }
}

// [[Rcpp::export]]
SEXP get_openmp_info() {
#ifdef _OPENMP
  return List::create(
    _["openmp_available"] = true,
    _["openmp_version"] = std::to_string(_OPENMP),
    _["max_threads"] = omp_get_max_threads()
  );
#else
  return List::create(
    _["openmp_available"] = false,
    _["openmp_version"] = "Not available",
    _["max_threads"] = 1
  );
#endif
}