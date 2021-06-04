// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/transducer.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// rbr_raw_to_pressure
arma::vec rbr_raw_to_pressure(const arma::vec& x, const arma::vec& calib);
static SEXP _transducer_rbr_raw_to_pressure_try(SEXP xSEXP, SEXP calibSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type calib(calibSEXP);
    rcpp_result_gen = Rcpp::wrap(rbr_raw_to_pressure(x, calib));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _transducer_rbr_raw_to_pressure(SEXP xSEXP, SEXP calibSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_transducer_rbr_raw_to_pressure_try(xSEXP, calibSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// rbr_raw_to_temperature
arma::vec rbr_raw_to_temperature(const arma::vec& x, const arma::vec& calib);
static SEXP _transducer_rbr_raw_to_temperature_try(SEXP xSEXP, SEXP calibSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type calib(calibSEXP);
    rcpp_result_gen = Rcpp::wrap(rbr_raw_to_temperature(x, calib));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _transducer_rbr_raw_to_temperature(SEXP xSEXP, SEXP calibSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_transducer_rbr_raw_to_temperature_try(xSEXP, calibSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// rbr_temperature_correction
arma::vec rbr_temperature_correction(const arma::vec& pressure, const arma::vec& temperature, const arma::vec& x);
static SEXP _transducer_rbr_temperature_correction_try(SEXP pressureSEXP, SEXP temperatureSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type pressure(pressureSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type temperature(temperatureSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rbr_temperature_correction(pressure, temperature, x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _transducer_rbr_temperature_correction(SEXP pressureSEXP, SEXP temperatureSEXP, SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_transducer_rbr_temperature_correction_try(pressureSEXP, temperatureSEXP, xSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// rbr_times
Rcpp::DatetimeVector rbr_times(const arma::vec& ev_tstamp, const arma::uvec& ev_index, double ti);
static SEXP _transducer_rbr_times_try(SEXP ev_tstampSEXP, SEXP ev_indexSEXP, SEXP tiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type ev_tstamp(ev_tstampSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type ev_index(ev_indexSEXP);
    Rcpp::traits::input_parameter< double >::type ti(tiSEXP);
    rcpp_result_gen = Rcpp::wrap(rbr_times(ev_tstamp, ev_index, ti));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _transducer_rbr_times(SEXP ev_tstampSEXP, SEXP ev_indexSEXP, SEXP tiSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_transducer_rbr_times_try(ev_tstampSEXP, ev_indexSEXP, tiSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// rbr_calib_mult
arma::mat rbr_calib_mult(arma::mat& x, const arma::mat& calib, const arma::vec& is_temp);
static SEXP _transducer_rbr_calib_mult_try(SEXP xSEXP, SEXP calibSEXP, SEXP is_tempSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type calib(calibSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type is_temp(is_tempSEXP);
    rcpp_result_gen = Rcpp::wrap(rbr_calib_mult(x, calib, is_temp));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _transducer_rbr_calib_mult(SEXP xSEXP, SEXP calibSEXP, SEXP is_tempSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_transducer_rbr_calib_mult_try(xSEXP, calibSEXP, is_tempSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// density_temperature
arma::vec density_temperature(const arma::vec& x);
static SEXP _transducer_density_temperature_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(density_temperature(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _transducer_density_temperature(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_transducer_density_temperature_try(xSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _transducer_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("arma::vec(*rbr_raw_to_pressure)(const arma::vec&,const arma::vec&)");
        signatures.insert("arma::vec(*rbr_raw_to_temperature)(const arma::vec&,const arma::vec&)");
        signatures.insert("arma::vec(*rbr_temperature_correction)(const arma::vec&,const arma::vec&,const arma::vec&)");
        signatures.insert("Rcpp::DatetimeVector(*rbr_times)(const arma::vec&,const arma::uvec&,double)");
        signatures.insert("arma::mat(*rbr_calib_mult)(arma::mat&,const arma::mat&,const arma::vec&)");
        signatures.insert("arma::vec(*density_temperature)(const arma::vec&)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _transducer_RcppExport_registerCCallable() { 
    R_RegisterCCallable("transducer", "_transducer_rbr_raw_to_pressure", (DL_FUNC)_transducer_rbr_raw_to_pressure_try);
    R_RegisterCCallable("transducer", "_transducer_rbr_raw_to_temperature", (DL_FUNC)_transducer_rbr_raw_to_temperature_try);
    R_RegisterCCallable("transducer", "_transducer_rbr_temperature_correction", (DL_FUNC)_transducer_rbr_temperature_correction_try);
    R_RegisterCCallable("transducer", "_transducer_rbr_times", (DL_FUNC)_transducer_rbr_times_try);
    R_RegisterCCallable("transducer", "_transducer_rbr_calib_mult", (DL_FUNC)_transducer_rbr_calib_mult_try);
    R_RegisterCCallable("transducer", "_transducer_density_temperature", (DL_FUNC)_transducer_density_temperature_try);
    R_RegisterCCallable("transducer", "_transducer_RcppExport_validate", (DL_FUNC)_transducer_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_transducer_rbr_raw_to_pressure", (DL_FUNC) &_transducer_rbr_raw_to_pressure, 2},
    {"_transducer_rbr_raw_to_temperature", (DL_FUNC) &_transducer_rbr_raw_to_temperature, 2},
    {"_transducer_rbr_temperature_correction", (DL_FUNC) &_transducer_rbr_temperature_correction, 3},
    {"_transducer_rbr_times", (DL_FUNC) &_transducer_rbr_times, 3},
    {"_transducer_rbr_calib_mult", (DL_FUNC) &_transducer_rbr_calib_mult, 3},
    {"_transducer_density_temperature", (DL_FUNC) &_transducer_density_temperature, 1},
    {"_transducer_RcppExport_registerCCallable", (DL_FUNC) &_transducer_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_transducer(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}