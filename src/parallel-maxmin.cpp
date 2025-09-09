#include <Rcpp.h>
#include <algorithm>
#include <RcppParallel.h>
using namespace RcppParallel;
using namespace Rcpp;


struct MaxMonth : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;
  
  // output matrix to write to
  RMatrix<double> rmat;
  
  MaxMonth(const NumericMatrix mat_1,
           NumericMatrix rmat)
    : mat_1(mat_1), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      
      RMatrix<double>::Row v1 = mat_1.row(i);
      
      //allocate the output for v1 + v2 matrix
      std::vector<double> output_1(mat_1.ncol());
      
      //Index for maximum of a matrix
      
      double result = std::max_element(v1.begin(), v1.end()) - v1.begin();
      //rmat(i, 0) = result;
      // this return the value itself
      rmat(i, 0) = v1[result];
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_which_max_row(NumericMatrix mat_1) {
  
  // allocate the matrix we will return,
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  MaxMonth maxmonth(mat_1, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), maxmonth);
  
  return rmat;
}


struct MinMonth : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;
  
  // output matrix to write to
  RMatrix<double> rmat;
  
  MinMonth(const NumericMatrix mat_1,
           NumericMatrix rmat)
    : mat_1(mat_1), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      
      RMatrix<double>::Row v1 = mat_1.row(i);
      
      //allocate the output for v1 + v2 matrix
      std::vector<double> output_1(mat_1.ncol());
      
      //Index for maximum of a matrix
      
      double result = std::min_element(v1.begin(), v1.end()) - v1.begin();
      //rmat(i, 0) = result;
      rmat(i, 0) = v1[result];
    }
  }
};


// [[Rcpp::export]]
NumericMatrix rcpp_parallel_which_min_row(NumericMatrix mat_1) {
  
  // allocate the matrix we will return,
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  MinMonth minmonth(mat_1, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), minmonth);
  
  return rmat;
}