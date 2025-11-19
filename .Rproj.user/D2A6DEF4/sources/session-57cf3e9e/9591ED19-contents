#include <Rcpp.h>
#include <algorithm>
#include <iostream>
#include <vector>
#include <numeric>
using namespace Rcpp;

#include <RcppParallel.h>
using namespace RcppParallel;


// generic function for variance
template <typename InputIterator>
inline double variance(InputIterator begin, InputIterator end) {
  
  // value to start
  double count = 0;
  double mean = 0;
  double delta = 0;
  double delta2 = 0;
  // value to return
  double M2 = 0;
  // set iterators to beginning of ranges
  InputIterator it = begin;
  // for each input item
  while (it != end) {
    
    // take the value and increment the iterator
    double new_value = *it++;
    
    // accumulate if appropriate
    count += 1;
    delta = new_value - mean;
    mean += delta/count;
    delta2 = new_value - mean;
    M2 += delta * delta2;
  }
  return M2;  
}



struct VarParallel : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;
  
  // output matrix to write to
  RMatrix<double> rmat;
  
  VarParallel(const NumericMatrix mat_1, NumericMatrix rmat)
    : mat_1(mat_1), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      
      RMatrix<double>::Row v1 = mat_1.row(i);
      
      double result = variance(v1.begin(), v1.end());
      
      rmat(i, 0) = result/(v1.size() - 1);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_variance(NumericMatrix mat_1) {
  
  // allocate the matrix we will return,
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  VarParallel varparallel(mat_1, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), varparallel);
  
  return rmat;
}

struct SDParallel : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;
  
  // output matrix to write to
  RMatrix<double> rmat;
  
  SDParallel(const NumericMatrix mat_1, NumericMatrix rmat)
    : mat_1(mat_1), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      
      RMatrix<double>::Row v1 = mat_1.row(i);
      
      double result = variance(v1.begin(), v1.end());
      
      rmat(i, 0) = sqrt(result/(v1.size() - 1));
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_sd(NumericMatrix mat_1) {
  
  // allocate the matrix we will return,
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  SDParallel sdparallel(mat_1, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), sdparallel);
  
  return rmat;
}