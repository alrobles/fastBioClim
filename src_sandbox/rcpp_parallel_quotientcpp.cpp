#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

#include <RcppParallel.h>
using namespace RcppParallel;

// helper function for taking the average of two numbers
inline double quotient(double val1, double val2) {
  return (val1 / val2);
}

struct matQuotParallel : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;
  const RMatrix<double> mat_2;
  
  // output matrix to write to
  RMatrix<double> rmat;
  
  matQuotParallel(const NumericMatrix mat_1, const NumericMatrix mat_2, NumericMatrix rmat)
    : mat_1(mat_1), mat_2(mat_2), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      // rows we will operate on
      RMatrix<double>::Row row1 = mat_1.row(i);
      RMatrix<double>::Row row2 = mat_2.row(i);
      
      // compute the quotient using std::tranform from the STL
      std::vector<double> quot(row1.length());
      std::transform(row1.begin(), row1.end(), // input range 1
                     row2.begin(),             // input range 2
                     quot.begin(),              // output range 
                     quotient);                 // function to apply
      
      
      rmat(i, 0) = quot[0];
      
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_quotient(NumericMatrix mat_1, NumericMatrix mat_2) {
  
  // allocate the matrix we will return,
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  matQuotParallel matquotparallel(mat_1, mat_2, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), matquotparallel);
  
  return rmat;
}