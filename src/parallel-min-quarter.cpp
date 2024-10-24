#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;


#include <RcppParallel.h>
using namespace RcppParallel;

// helper function for taking the average of two numbers
inline double plus_two(double val1, double val2) {
  return (val1 + val2);
}


struct MinQuarter : public Worker {
  // input matrix to read from
  const RMatrix<double> mat_1;
  // input matrix to read from
  const RMatrix<double> mat_2;
  // input matrix to read from
  const RMatrix<double> mat_3;
  
  // output matrix to write to
  RMatrix<double> rmat;
  
  MinQuarter(const NumericMatrix mat_1,
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
      
      //Index for Minimum of the sum of a quarter
      //int Min = matrixMinInd(output_2);
      
      double result = std::min_element(output_2.begin(), output_2.end()) - output_2.begin();
      rmat(i, 0) = result;
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_which_min_quarter(NumericMatrix mat_1, NumericMatrix mat_2, NumericMatrix mat_3) {
  
  // allocate the matrix we will return,
  NumericMatrix rmat(mat_1.nrow(), 1);
  
  // create the worker
  MinQuarter minquarter(mat_1, mat_2, mat_3, rmat);
  
  // call it with parallelFor
  parallelFor(0, mat_1.nrow(), minquarter);
  
  return rmat;
}
