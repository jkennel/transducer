#' adjust_time
#'
#'
#' @param x data.table of transducer data
#' @param n_sec number of seconds to adjust
#'
#' @return
#' @export
#'
#' @examples
adjust_time <- function(x, n_sec = 0) {
  x[, data := lapply(data, function(x) x[, datetime := datetime + n_sec])]
  x
}
