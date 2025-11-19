#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
using namespace Rcpp;
using namespace RcppParallel;

// Difference function
inline double difference(double val1, double val2) {
  return val1 - val2;
}

struct matDiffParallel : public Worker {
  const RMatrix<double> mat_1;
  const RMatrix<double> mat_2;
  RMatrix<double> rmat;
  
  matDiffParallel(const NumericMatrix mat_1, const NumericMatrix mat_2, NumericMatrix rmat)
    : mat_1(mat_1), mat_2(mat_2), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      RMatrix<double>::Row row1 = mat_1.row(i);
      RMatrix<double>::Row row2 = mat_2.row(i);
      RMatrix<double>::Row rowOut = rmat.row(i);
      
      std::transform(row1.begin(), row1.end(), row2.begin(), rowOut.begin(), difference);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_difference(NumericMatrix mat_1, NumericMatrix mat_2) {
  if (mat_1.nrow() != mat_2.nrow() || mat_1.ncol() != mat_2.ncol())
    stop("Matrices must have the same dimensions");
  
  NumericMatrix rmat(mat_1.nrow(), mat_1.ncol());
  matDiffParallel worker(mat_1, mat_2, rmat);
  parallelFor(0, mat_1.nrow(), worker);
  return rmat;
}
