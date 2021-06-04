#===============================================================================
#' make_regular
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
#===============================================================================
make_regular <- function(dat) {

  start <- min(sapply(dat$data, function(x) head(x, 1)$datetime), na.rm = TRUE)
  end   <- max(sapply(dat$data, function(x) tail(x, 1)$datetime), na.rm = TRUE)

  by = dat$by[1]
  dates <- data.table(datetime = seq.int(start, end, by = by))

  dat[, data := lapply(data, function(x) x[dates, on = 'datetime'])]

  invisible(dat)

}

# library(transducer)
# dat <- read_rbr('/media/kennel/Seagate Expansion Drive/rbr_ssfl/rd45b 081872_20191118_1154.rsk', by = 3600)
