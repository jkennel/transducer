#define BOOST_DISABLE_ASSERTS
#define ARMA_DONT_PRINT_ERRORS
#define ARMA_NO_DEBUG
// #define ARMA_USE_TBB_ALLOC

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
// [[Rcpp::interfaces(r, cpp)]]

//' rbr_raw_to_pressure
//'
//' @param x
//' @param calib
//'
//' @return
//' @export
//'
//' @examples
// [[Rcpp::export]]
arma::vec rbr_raw_to_pressure(const arma::vec& x,
                              const arma::vec& calib) {

  arma::vec z = arma::polyval(arma::reverse(calib), x);

  return(z);

}

//' rbr_raw_to_temperature
//'
//' @param x
//' @param calib
//'
//' @return
//' @export
//'
//' @examples
// [[Rcpp::export]]
arma::vec rbr_raw_to_temperature(const arma::vec& x,
                                 const arma::vec& calib) {

  double k_to_c = 273.15;
  arma::vec z = arma::log(1.0 / x - 1.0);

  z = arma::polyval(arma::reverse(calib), z);
  z = 1.0 / z - k_to_c;

  return(z);

}


//' rbr_temperature_correction
//'
//' @param pressure
//' @param temperature
//' @param x calibration constants
//'
//' @return
//' @export
//'
//' @examples
// [[Rcpp::export]]
arma::vec rbr_temperature_correction(const arma::vec& pressure,
                                     const arma::vec& temperature,
                                     const arma::vec& x) {

  arma::vec tcal = temperature - x(5);
  arma::vec out = pressure - x(0);

  arma::vec co = arma::reverse(x.subvec(0, 3));
  co(3) = 0.0;

  out -= arma::polyval(co, tcal);

  out = x(0) + out / (1.0 + x(4) * tcal);

  return(out);
}

//' rbr_times
//'
//' @param ev_tstamp
//' @param ev_index
//' @param ti
//'
//' @return
//' @export
//'
//' @examples
// [[Rcpp::export]]
Rcpp::DatetimeVector rbr_times(const arma::vec& ev_tstamp,
                               const arma::uvec& ev_index,
                               double ti) {

  size_t n_ev = ev_tstamp.n_elem - 1;
  int dif, s, e;

  arma::rowvec ep = ev_tstamp;
  arma::rowvec a(max(ev_index) - n_ev);

  for(size_t j = 0; j < n_ev; j++) {
    s = ev_index[j] - 1;
    e = ev_index[j+1] - j - 1;
    dif = e - s;
    a.subvec(s, e) = ep[j] + arma::linspace(0, dif, dif + 1) * ti;
  }

  Rcpp::DatetimeVector dt(Rcpp::NumericVector(a.begin(),a.end()));
  dt.attr("tzone") = "UTC";
  return(dt);
}




//' rbr_calib_mult
//'
//' @param x
//' @param calib
//' @param is_temp
//'
//' @return
//' @export
//'
//' @examples
// [[Rcpp::export]]
arma::mat rbr_calib_mult(arma::mat& x,
                         const arma::mat& calib,
                         const arma::vec& is_temp) {

  double k_to_c = 273.15;

  arma::uword nr = x.n_rows;
  arma::uword nc = x.n_cols;

  arma::mat z(nr, nc);
  arma::vec co;

  for(arma::uword j = 0; j < nc; j++) {

    if(is_temp(j)) {
      x.col(j) = arma::log(1.0 / x.col(j) - 1.0);
    }

    co = arma::reverse(calib.row(j));

    z.col(j) = arma::polyval(co, x.col(j));

    if(is_temp(j)) {
      z.col(j) = 1.0 / z.col(j) - k_to_c;
    }

  }


  return(z);

}


//' @title  density_temperature
//'
//' @details This function estimates the density of water at a given temperature
//' using Kell's formulation. ITS-90
//'
//' @param x temperature
//'
//' @return
//' @export
//' @seealso \link{https://nvlpubs.nist.gov/nistpubs/jres/097/jresv97n3p335_A1b.pdf}
// [[Rcpp::export]]
arma::vec density_temperature(const arma::vec& x) {

  arma::vec calib{280.54253e-12, 105.56302e-9, -46.170461e-6,
                  -7.9870401e-3,    16.945176,     999.83952};

  return(arma::polyval(calib, x) / (1.0 + 16.897850e-3 * x));
}




// // [[Rcpp::export]]
// arma::vec not_subset(const arma::vec& co,
//                      const arma::vec& x) {
//
//   return(arma::polyval(co, x));
//
// }
// // this is close to r subseting speed
// // [[Rcpp::export]]
// arma::vec anti_subset(arma::vec& x, arma::uvec& idx) {
//   x.elem(idx).fill(NA_REAL);
//   return(x.elem(find_finite(x))/pow(2.0, 30));
// }

/*** R
x <- rnorm(1e4)
microbenchmark::microbenchmark(density_temperature(x),
                               times = 10)
#
# tmp <- rnorm(4e7)
# system.time(a <- anti_subset(tmp, c(10, 20)-1))
# system.time(b <- tmp[-c(10, 20)])
# aaa <- 1:3.9e7
# system.time(a <- tmp[aaa])

# system.time(bb <- rbr_times(ev$tstamp/1000, ev$sampleIndex, ti))
# system.time(cc <- .rbr_times(ev, ti))

# calib_mult_2 <- function(x, cal) {
#
#   z <- rep(cal[1], length(x))
#
#   x2 <- x*x
#   x3 <- x*x2
#
#   z <- z + x*cal[2] + x2*cal[3] + x3*cal[4]
#
#   z
# }
#
# calib_mult_3 <- function(x, cal) {
#
#   z <- rep(cal[1], length(x))
#
#   x2 <- x*x
#   x3 <- x*x2
#
#   z <- z + x*cal[2] + x*x*cal[3] + x*x*x*cal[4]
#
#   z
# }
#
#
# y <- as.matrix(val)
# head(y)
# is_temp <- c(FALSE, TRUE)
# microbenchmark::microbenchmark(
#   tmp <- rbr_calib_mult(y, co, is_temp),
#   b <- rbr_temperature_correction(tmp[,1], tmp[,2], x0),
#   # calib_mult_2(y, cal),
#   # calib_mult_3(y, cal),
#   times = 2
# )




*/
