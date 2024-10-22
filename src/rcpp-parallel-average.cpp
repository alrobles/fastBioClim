#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

#include <RcppParallel.h>
using namespace RcppParallel;

struct MeanParallel : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;

  // output matrix to write to
  RMatrix<double> rmat;
  
  MeanParallel(const NumericMatrix mat_1, NumericMatrix rmat)
    : mat_1(mat_1), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      
      RMatrix<double>::Row v1 = mat_1.row(i);
      double result = std::accumulate(v1.begin(), v1.end(), 0.0)/v1.size();
      rmat(i, 0) = result;
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_average(NumericMatrix mat_1) {
  
  // allocate the matrix we will return,
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  MeanParallel meanparallel(mat_1, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), meanparallel);
  
  return rmat;
}