#===============================================================================
# sometimes outputs are not on even times
#' make_even_times
#'
#' @param x
#' @param max_difference
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#===============================================================================
make_even_times <- function(x, diff_time,  ...) {

  # may want to use the least common multiple
  dates <- as.POSIXct((as.numeric(x$datetime) %/% diff_time) * diff_time, tz = 'UTC', origin = '1970-01-01')
  dates <- data.table(datetime = c(dates, max(dates, na.rm = TRUE) + diff_time))
  dates <- unique(dates)
  dates <- dates[, datetime_reg := datetime]

  z <- dates[x, on = 'datetime', roll = 'nearest']
  z[, datetime_diff := as.numeric(datetime_reg) - as.numeric(datetime)]
  setkey(z, datetime)
  z[, datetime := NULL]
  setnames(z, 'datetime_reg', 'datetime')

  z

}
