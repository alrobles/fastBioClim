// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// matrixMinInd
int matrixMinInd(NumericMatrix x);
RcppExport SEXP _fastBioClim_matrixMinInd(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(matrixMinInd(x));
    return rcpp_result_gen;
END_RCPP
}
// matrixMaxInd
int matrixMaxInd(NumericMatrix x);
RcppExport SEXP _fastBioClim_matrixMaxInd(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(matrixMaxInd(x));
    return rcpp_result_gen;
END_RCPP
}
// vectorMaxInd
int vectorMaxInd(NumericVector x);
RcppExport SEXP _fastBioClim_vectorMaxInd(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(vectorMaxInd(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_which_max_quarter
NumericMatrix rcpp_which_max_quarter(NumericMatrix mat);
RcppExport SEXP _fastBioClim_rcpp_which_max_quarter(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_which_max_quarter(mat));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_parallel_which_max_quarter
NumericMatrix rcpp_parallel_which_max_quarter(NumericMatrix mat_1, NumericMatrix mat_2, NumericMatrix mat_3);
RcppExport SEXP _fastBioClim_rcpp_parallel_which_max_quarter(SEXP mat_1SEXP, SEXP mat_2SEXP, SEXP mat_3SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat_1(mat_1SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mat_2(mat_2SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mat_3(mat_3SEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_parallel_which_max_quarter(mat_1, mat_2, mat_3));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_max_quarter
NumericMatrix rcpp_get_max_quarter(NumericMatrix maxQuarter, NumericMatrix mat);
RcppExport SEXP _fastBioClim_rcpp_get_max_quarter(SEXP maxQuarterSEXP, SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type maxQuarter(maxQuarterSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_max_quarter(maxQuarter, mat));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_parallel_average
NumericMatrix rcpp_parallel_average(NumericMatrix mat_1);
RcppExport SEXP _fastBioClim_rcpp_parallel_average(SEXP mat_1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat_1(mat_1SEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_parallel_average(mat_1));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_parallel_variance
NumericMatrix rcpp_parallel_variance(NumericMatrix mat_1);
RcppExport SEXP _fastBioClim_rcpp_parallel_variance(SEXP mat_1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat_1(mat_1SEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_parallel_variance(mat_1));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_parallel_sd
NumericMatrix rcpp_parallel_sd(NumericMatrix mat_1);
RcppExport SEXP _fastBioClim_rcpp_parallel_sd(SEXP mat_1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat_1(mat_1SEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_parallel_sd(mat_1));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fastBioClim_matrixMinInd", (DL_FUNC) &_fastBioClim_matrixMinInd, 1},
    {"_fastBioClim_matrixMaxInd", (DL_FUNC) &_fastBioClim_matrixMaxInd, 1},
    {"_fastBioClim_vectorMaxInd", (DL_FUNC) &_fastBioClim_vectorMaxInd, 1},
    {"_fastBioClim_rcpp_which_max_quarter", (DL_FUNC) &_fastBioClim_rcpp_which_max_quarter, 1},
    {"_fastBioClim_rcpp_parallel_which_max_quarter", (DL_FUNC) &_fastBioClim_rcpp_parallel_which_max_quarter, 3},
    {"_fastBioClim_rcpp_get_max_quarter", (DL_FUNC) &_fastBioClim_rcpp_get_max_quarter, 2},
    {"_fastBioClim_rcpp_parallel_average", (DL_FUNC) &_fastBioClim_rcpp_parallel_average, 1},
    {"_fastBioClim_rcpp_parallel_variance", (DL_FUNC) &_fastBioClim_rcpp_parallel_variance, 1},
    {"_fastBioClim_rcpp_parallel_sd", (DL_FUNC) &_fastBioClim_rcpp_parallel_sd, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_fastBioClim(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}