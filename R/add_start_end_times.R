#===============================================================================
# simplify to first and last value for speed
#' add_start_end_times
#'
#' @param x data.table of transducer data
#'
#' @return data.table with starting and end times for each dataset
#' @export
#'
#' @examples
#===============================================================================
add_start_end_times <- function(x) {

  x[, start := head(data[[1]], 1)$datetime, by = list(file, channel, serial)]
  x[, end := tail(data[[1]], 1)$datetime, by = list(file, channel, serial)]

  x

}
