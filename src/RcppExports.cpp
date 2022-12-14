// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// chop
NumericMatrix chop(NumericVector x1, NumericVector x2, NumericVector y1, NumericVector y2, NumericVector p, double value, double m_cost, double t_cost, double mobility);
RcppExport SEXP _deforecasteR_chop(SEXP x1SEXP, SEXP x2SEXP, SEXP y1SEXP, SEXP y2SEXP, SEXP pSEXP, SEXP valueSEXP, SEXP m_costSEXP, SEXP t_costSEXP, SEXP mobilitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x2(x2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y2(y2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type value(valueSEXP);
    Rcpp::traits::input_parameter< double >::type m_cost(m_costSEXP);
    Rcpp::traits::input_parameter< double >::type t_cost(t_costSEXP);
    Rcpp::traits::input_parameter< double >::type mobility(mobilitySEXP);
    rcpp_result_gen = Rcpp::wrap(chop(x1, x2, y1, y2, p, value, m_cost, t_cost, mobility));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_deforecasteR_chop", (DL_FUNC) &_deforecasteR_chop, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_deforecasteR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
