#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

// helper function for taking the average of two numbers
inline double plus_two(double val1, double val2) {
  return (val1 + val2);
}

// [[Rcpp::export]]
int matrixMinInd(NumericMatrix x) {
  // Rcpp supports STL-style iterators
  NumericMatrix::iterator it = std::min_element(x.begin(), x.end());
  // we want the value so dereference 
  return it - x.begin();
}

// [[Rcpp::export]]
int matrixMaxInd(NumericMatrix x) {
  // Rcpp supports STL-style iterators
  NumericMatrix::iterator it = std::max_element(x.begin(), x.end());
  // we want the value so dereference 
  return it - x.begin();
}

// [[Rcpp::export]]
int vectorMaxInd(NumericVector x) {
  // Rcpp supports STL-style iterators
  NumericVector::iterator it = std::max_element(x.begin(), x.end());
  // we want the value so dereference 
  return it - x.begin();
}

// [[Rcpp::export]]
NumericMatrix rcpp_which_max_quarter(NumericMatrix mat) {
  
  // allocate the matrix we will return
  NumericMatrix rmat(mat.nrow(), 1);
  
  for (int i = 0; i < mat.nrow(); i++) {
    NumericMatrix v1 = mat( Range(i, i),  Range(0, mat.ncol() - 3));
    NumericMatrix v2 = mat( Range(i, i),  Range(1, mat.ncol() - 2));
    NumericMatrix v3 = mat( Range(i, i),  Range(2, mat.ncol() - 1));
    
    // allocate the output matrix
    NumericMatrix output_1(1, mat.ncol() - 2 );
    
    // allocate the output matrix
    NumericMatrix output_2(1, mat.ncol() - 2 );
    
    std::transform(v1.begin(), v1.end(),
                   v2.begin(),
                   output_1.begin(),
                   plus_two);
    // 
    std::transform(output_1.begin(), output_1.end(),
                   v3.begin(),
                   output_2.begin(),
                   plus_two);
    
    int max = matrixMaxInd(output_2);
    
    rmat(i, 0) = max;  
  }
  
  return rmat;
}

#include <RcppParallel.h>
using namespace RcppParallel;

struct MaxQuarter : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;
  // input matrix to read from
  const RMatrix<double> mat_2;
  // input matrix to read from
  const RMatrix<double> mat_3;
  
  // output matrix to write to
  RMatrix<double> rmat;
  
  MaxQuarter(const NumericMatrix mat_1,
               const NumericMatrix mat_2,
               const NumericMatrix mat_3,
               NumericMatrix rmat)
    : mat_1(mat_1), mat_2(mat_2), mat_3(mat_3), rmat(rmat) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      
      RMatrix<double>::Row v1 = mat_1.row(i);
      RMatrix<double>::Row v2 = mat_2.row(i);
      RMatrix<double>::Row v3 = mat_3.row(i);
      
      //allocate the output for v1 + v2 matrix
      std::vector<double> output_1(mat_1.ncol());
      
      // allocate the output for v1 + v2 + v3 matrix
      //NumericMatrix output_2(1, mat_1.ncol() - 2 );
      std::vector<double> output_2(mat_1.ncol());
      
      //apply plus two with v1 and v2
      std::transform( v1.begin(), v1.end(),
                      v2.begin(),
                      output_1.begin(),
                      plus_two);
      
      //  apply plus two with the third vector
      std::transform(output_1.begin(), output_1.end(),
                     v3.begin(),
                     output_2.begin(),
                     plus_two);
      
      //Index for maximum of the sum of a quarter
      //int max = matrixMaxInd(output_2);
      
      double result = std::max_element(output_2.begin(), output_2.end()) - output_2.begin();
      rmat(i, 0) = result;
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_which_max_quarter(NumericMatrix mat_1, NumericMatrix mat_2, NumericMatrix mat_3) {
  
  // allocate the matrix we will return,
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  MaxQuarter maxquarter(mat_1, mat_2, mat_3, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), maxquarter);
  
  return rmat;
}