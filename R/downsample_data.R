#' downsample_data
#'
#' @param x the data.table of transducer readings
#' @param time_interval the time interval in seconds
#'
#' @return data.table of subsampled data
#' @export
#'
#' @examples
downsample_data <- function(x, time_interval = 3600) {

  x[, data := lapply(data, function(x) x[(as.numeric(datetime) %% time_interval) == 0])]

  x[, dt := time_interval]

  x
}


#' subset_time_interval
#'
#' @param x the data.table of transducer readings
#' @param start beginning of subset (POSIXct)
#' @param end end of subset (POSIXct)
#'
#' @return data.table with values between start and end
#' @export
#'
#' @examples
subset_time_interval <- function(x, start = NULL, end = NULL) {

  if(is.null(start) & is.null(end)) {
    # do nothing
  } else if(is.null(start)) {
    x[, data := lapply(data, function(x) x[datetime < end])]
  } else if(is.null(end)) {
    x[, data := lapply(data, function(x) x[datetime > start])]

  } else {
    x[, data := lapply(data, function(x) x[between(datetime, start, end)])]
  }

  x
}







# Unfinished --------------------------------------------------------------


#' combine_files
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
combine_files <- function(x) {

  # check units

  class(x$file) <- 'character'

  tmp <- x[, list(data = list(rbindlist(data))),
           by = list(channel, id, parameter, units, version, serial, model, dt)]

  tmp[, file := 'interpolated']

  tmp[, calibration := list(data.table(coef = character(), value = numeric()))]


  tmp[, data := lapply(data, function(y) {
    # print(y)
    if(nrow(y) != 0){
      setkey(y, datetime)
      dates <- data.table(datetime = seq.POSIXt(y[1]$datetime, y[.N]$datetime, 3600))
      y <- y[dates, on = 'datetime']

    }

    setkey(y, datetime)

    return(y[is.na(value)])
  })]

  setcolorder(tmp, c("file", "channel", "data", "id", "calibration",
                     "parameter", "units", "version", "serial", "model", "dt"))

  rbind(x, tmp)


}






#' fill_gaps
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
fill_gaps <- function(x) {


  tmp <- x[, list(data = list(rbindlist(data))), by = list(serial, channel)]


  tmp[, data := lapply(data, function(y) {
    if(nrow(y) != 0){

      dates <- data.table(datetime = seq.POSIXt(y[1]$datetime, y[.N]$datetime, 3600))
      y <- y[dates, on = 'datetime']

    }

    setkey(y, datetime)

    return(y[is.na(value)])
  })]

  sum(sapply(x$data, nrow))
  unlist(sapply(x$data, function(y) unique(diff(y$datetime))))
  sum(sapply(dat$data, nrow))

}


#' filter_downloads
#'
#' @param x
#' @param cutoff
#'
#' @return
#' @export
#'
#' @examples
filter_downloads <- function(x, cutoff = 1050) {


  copy(x)[, data := lapply(data, function(y) {
    d <- abs(diff(y$value))
    s <- mad(d, na.rm=TRUE)
    m <- median(d, na.rm=TRUE)
    print(s)
    print(head(d))
    print(m)
    y[c(d[1], d) > m + (s * 20), value := NA_real_]
    y
  })]


}
