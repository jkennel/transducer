#===============================================================================
#' westbay_to_elevation
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#===============================================================================
westbay_to_elevation <- function (x, ...) {
  UseMethod("westbay_to_elevation", x)
}


#===============================================================================
#' westbay_to_elevation
#'
#' @param x
#' @param baro_port
#'
#' @return
#' @export
#'
#' @examples
#===============================================================================
westbay_to_elevation.data.table <- function(x, baro_port = 0) {

  baro <- x[port == baro_port]
  baro <- baro[, data[[1]], by = 1:nrow(baro)]
  baro <- unique(baro[, list(datetime, baro = value)])

  # add barometric pressure data
  x[, data := lapply(x$data, function(z) z[baro, on = 'datetime', nomatch = 0])]

  if(!'port_depth' %in% names(x)){
    x[, port_depth := transducer_depth]
  }

  # do conversion
  x[, data := list(.(westbay_to_elevation(well_elevation,
                                          transducer_depth,
                                          port_depth,
                                          data,
                                          units))), by = 1:nrow(x)]





  return(x)

}




#===============================================================================
#' westbay_to_elevation
#'
#' @param x
#' @param port_depth
#' @param data
#' @param units
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#===============================================================================
westbay_to_elevation.numeric <- function(x, transducer_depth, port_depth, data, units, ...) {

  a <- copy(data[[1]])

  if (units == 'm') {
    psia_conv = 1.4219702063247
  } else if (units == 'ft') {
    psia_conv = 0.43352750192825
  } else {
    psia_conv = 1.0
  }


  a[, wl_elevation := x - transducer_depth + (value - baro) / psia_conv]
  a[, port_elevation := x - port_depth]
  a

}


#===============================================================================
# sometimes outputs are not on even times
#' westbay_make_regular
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
westbay_make_regular <- function(x, max_difference = NULL,  ...) {

  # may want to use the least common multiple
  n_sec <- max(x$dt)

  if(is.null(max_difference)) {
    max_difference <- n_sec / 4
  }

  if(!'start' %in% names(x)) {
    x[, start := min(data[[1]]$datetime), by = 1:nrow(x)]
    x[, end := max(data[[1]]$datetime), by = 1:nrow(x)]
  }
  mn <- min(x[['start']], na.rm = TRUE)
  mx <- max(x[['end']], na.rm = TRUE)


  dates <- data.table(datetime = seq.POSIXt(mn, mx, n_sec))
  dates <- dates[, datetime_reg := datetime]

  x[, data := lapply(x$data, function(z) {

    z <- dates[z, on = 'datetime', roll = 'nearest']
    z <- z[, datetime_diff := as.numeric(datetime_reg) - as.numeric(datetime)]
    setkey(z, datetime)
    z <- z[datetime_diff < max_difference]
    z[, datetime := NULL]
    setnames(z, 'datetime_reg', 'datetime')


    z
  })]

  x

}





#===============================================================================
#' westbay_interpolate
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#===============================================================================
westbay_interpolate <- function(x) {

  if(!'start' %in% names(x)) {
    x[, start := min(data[[1]]$datetime), by = 1:nrow(x)]
    x[, end := max(data[[1]]$datetime), by = 1:nrow(x)]
  }
  mn <- min(x[['start']], na.rm = TRUE)
  mx <- max(x[['end']], na.rm = TRUE)

  tmp    <- x[, (data[[1]]), by = list(port, start, end)]
  # tmp[, file := NULL]
  dates  <- data.table(datetime = seq.POSIXt(mn, mx, x$dt[1]))
  tmp <- tmp[, .SD[dates, on = 'datetime'],  by = list(port)]
  setkey(tmp, datetime)

  # Interpolate numeric columns
  nms <- names(tmp)[sapply(tmp, is.numeric)]
  nms <- nms[!nms %in% 'datetime_diff']
  nms <- nms[!nms %in% 'port']
  tmp[ , (nms) := (lapply(.SD, function(z) {
    approx(datetime, z, datetime, ties = 'ordered')$y})),
       by = port, .SDcols = nms]

  to_add <- tmp[is.na(datetime_diff)]

  to_add <- to_add[, .(data = list(.SD)), by = list(port)]

  # order the data columns
  name_order <- names(x$data[[1]])
  to_add[, data := lapply(data, function(x) x[, name_order, with = FALSE])]


  meta   <- unique(x[, list(channel, id, parameter, units, version,
                  serial, model, dt, transducer_depth, well_elevation,
                  probe_id, port, probe_description, port_description)])
  to_add <- to_add[meta, on = c('port')]

  to_add[, start := min(data[[1]]$datetime), by = 1:nrow(to_add)]
  to_add[, end := max(data[[1]]$datetime), by = 1:nrow(to_add)]
  to_add[, file := NA_character_]
  to_add[, calibration := list(data.table(coef = character(), value = numeric()))]
  data.table::setcolorder(to_add, names(x))


#   start <- min(to_add$datetime)
#   end   <- max(to_add$datetime)
#   n     <- nrow(to_add)
#
#   to_add <- data.table(
#     file = 'interpolated',
#     channel = x$channel[1],
#     data = list(to_add),
#     id = NA,
#     elev = x$elev[1],
#     units = x$units[1],
#     dt = x$dt[1],
#     start,
#     end,
#     n
#   )

  rbind(x, to_add)

}


# library(transducer)
# library(aquifer)
# library(ggplot2)
# library(waterlevel)
# db_name <- '/home/jonathankennel/Storage/data/westbay/mosdax'
# fn <- list.files(db_name, full.names = TRUE)
# y <- read_westbay(fn)
#
# # conversion because elevation is incorrect
# y[, elev := elev / 0.3048]
#
# y <- westbay_make_regular(y)
# y <- westbay_to_elevation(y)
# y <- westbay_interpolate(y)
#
# aa <- unique(y[, (data[[1]]), by = file])
#
# setkey(aa, datetime)
#
# ggplot(aa, aes(y = wl_elevation, x = datetime)) +
#   geom_line() +
#   facet_wrap(port_elevation~., nrow = 5, scales = 'free_y') +
#   theme_bw()
#
# #
# #
# ggplot(aa, aes(y = wl_elevation, x = port_elevation, color = datetime, group = datetime)) +
#   geom_line(alpha = 0.1, size = 0.5) +
#   geom_point() +
#   # scale_x_reverse() +
#   coord_flip() +
#   scale_color_viridis_c() +
#   theme_bw()
#
# ggplot(aa[port == 5], aes(y = wl_elevation, x = datetime)) +
#   geom_line() +
#   scale_x_continuous(expand = c(0,0)) +
#   facet_wrap(port_elevation~., nrow = 5) +
#   theme_bw()
#
#
# library(waterlevel)
# library(earthtide)
#
# lat <- 43.46283
# lon <- -80.21921
#
# wg <- setDT(earthtide::eterna_wavegroups)
# wg <- wg[time=='all']
# tmp <- aa[port == 9]
# tmp <- unique(tmp[, list(datetime, pressure, wl_elevation, ba)])
# tmp <- na.omit(tmp)
# tmp[, et := calc_earthtide(utc = datetime,
#                      latitude = lat,
#                      method = 'gravity',
#                      longitude = lon, cutoff = 1e-6,
#                      wave_groups = wg)$gravity]
#
# a <- transfer_fun(tmp, c('pressure', 'ba', 'et'), method = 'spec_pgram', spans = 9)
# plot(et~frequency, a, type='l', xlim = c(0.8, 3), col = 'green')
# par(new = TRUE)
# plot(pressure~frequency, a, type='l', xlim = c(0.8, 3), ylim = c(0, 0.3))
# # points(ba~frequency, a, type='l', xlim = c(0.8, 3), ylim = c(0, 0.5), col = 'red')
# abline(v = 1.93)
# abline(v = 0.93)
#
# # tmp <- tmp[seq(1, nrow(tmp), 2)]
#
# plot(gain_pressure_et~frequency, a, type='l', xlim = c(0.8, 2.2), ylim = c(0, 1e-4))
# plot(180/pi*phase_pressure_et~frequency, a, type='l', xlim = c(0.8, 2.2))
# abline(h = c(-60, -0))
#
# plot(gain_pressure_ba~frequency, a, type='l', xlim = c(0.0, 3), ylim = c(0,2))
#
# plot(180/pi*phase_pressure_ba~frequency, a, type='l', xlim = c(0.0, 5))
# abline(h = c(-180, 180))
#

