#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

inline double nansum (const double a, const double b)
{
  return a + (std::isnan(b) ? 0 : b);
}

// [[Rcpp::export]]
NumericMatrix rcpp_get_max_quarter(NumericMatrix maxQuarter, NumericMatrix mat) {
  
  // allocate the matrix we will return
  NumericMatrix rmat(mat.nrow(), 1);
  
  for (int i = 0; i < mat.nrow(); i++) {
    double indexQuarter = maxQuarter(i, 0);
    
    
    double x1 = mat(i, indexQuarter);
    double x2 = mat(i, indexQuarter + 1);
    double x3 = mat(i, indexQuarter + 2);
    
    std::vector<double> vec = {x1, x2, x3};
    
    double result =  std::accumulate(vec.begin(),
                                     vec.end(),
                                     0.0, nansum);
    double result_n =  std::count_if(vec.begin(), vec.end(),
                                     [](double i) { return std::isnan(i); });
    
    
    // compute the average 
    double avg = result/(vec.size() - result_n);
    
    rmat(i, 0) = avg;
  }
  
  return rmat;
}