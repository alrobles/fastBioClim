#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

#include <RcppParallel.h>
using namespace RcppParallel;

// helper function for taking the average of two numbers
inline double difference(double val1, double val2) {
  return (val1 - val2);
}

struct matDiffParallel : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;
  const RMatrix<double> mat_2;
  
  // output matrix to write to
  RMatrix<double> rmat;
  
  matDiffParallel(const NumericMatrix mat_1, const NumericMatrix mat_2, NumericMatrix rmat)
    : mat_1(mat_1), mat_2(mat_2), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      // rows we will operate on
      RMatrix<double>::Row row1 = mat_1.row(i);
      RMatrix<double>::Row row2 = mat_2.row(i);
      
      // compute the average using std::tranform from the STL
      std::vector<double> diff(row1.length());
      std::transform(row1.begin(), row1.end(), // input range 1
                     row2.begin(),             // input range 2
                     diff.begin(),              // output range 
                     difference);                 // function to apply
      
      //double result = std::accumulate(v1.begin(), v1.end(), 0.0)/v1.size();
      rmat(i, 0) = diff[0];
      
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_difference(NumericMatrix mat_1, NumericMatrix mat_2) {
  
  // allocate the matrix we will return,
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  matDiffParallel matdiffparallel(mat_1, mat_2, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), matdiffparallel);
  
  return rmat;
}