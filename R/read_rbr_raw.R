#' read_rbr_raw
#'
#' @param x (character) name of the rsk file
#' @param type (character vector) one or more of 'raw', 'basic', or 'compensated'
#'
#' @return
#' @export
#'
#' @examples
read_rbr_raw <- function(x, type = c('raw', 'basic', 'compensated')) {

  # https://docs.rbr-global.com/L3commandreference/format-of-stored-data/standard-rawbin00-format/standard-format-events-markers
  # Seconds for the date/time are counted from 2000-01-01T00:00:00Z.

  x <- "/media/jonathankennel/Seagate Expansion Drive/rbr_g360/075658_MW13A-20_T_3.rsk"
  # setup database connection
  db <- RSQLite::dbConnect(RSQLite::SQLite(), x)
  RSQLite::dbListTables(conn = db)

  lapply(RSQLite::dbListTables(conn = db), function(x) {
    str(RSQLite::dbGetQuery(db, paste0("SELECT * FROM ", x)))
  })


  # continuous table (time interval)
  ti <- RSQLite::dbGetQuery(db, "SELECT samplingPeriod FROM continuous")[[1]]/1000

  # epochs table (start time)
  ep <- RSQLite::dbGetQuery(db, "SELECT startTime/1000 FROM epochs")[[1]]
  ep <- as.POSIXct(ep, origin = '1970-01-01', tz = 'UTC')

  # instruments table
  im <- RSQLite::dbGetQuery(db, "SELECT * FROM instruments")

  # coefficient table
  co <- data.table::as.data.table(RSQLite::dbGetQuery(db, "SELECT * FROM coefficients"))
  # cal <- data.table::as.data.table(RSQLite::dbGetQuery(db, "SELECT * FROM calibrations"))

  # channel table
  ch <- RSQLite::dbGetQuery(db, "SELECT * FROM channels")

  # errors table
  er <- RSQLite::dbGetQuery(db, "SELECT * FROM errors")

  # events table
  ev <- RSQLite::dbGetQuery(db, "SELECT * FROM events")

  # raw table
  dl <- RSQLite::dbGetQuery(db, "SELECT data FROM downloads")[[1]]

  RSQLite::dbDisconnect(db)


  # get coefficients
  x0 <- .get_correction_coef(co)
  cs <- .get_base_coef(co)


  # get dimensions and names
  n_chan  <- nrow(ch)
  ch_name <- tolower(ch$longName)

  # temperature correction for pressure
  pressure_index    <- as.numeric(co[grepl('n0', key)]$calibrationID)
  temperature_index <- as.numeric(co[grepl('n0', key)]$value)


  if (length(pressure_index) > 0 & length(temperature_index) > 0) {
    ch_name[temperature_index] <- paste0(ch_name[temperature_index], '_onboard')
  }


  # read binary data
  val_i <- unlist(lapply(dl, .rbr_parse))


  # remove header and create matrix
  t1     <- as.numeric(ev$tstamp)[[1]] * 1e-3 -
            as.numeric(as.POSIXct('2000-01-01 00:00:00', tz = 'UTC'))
  h_len  <- max(which(val_i[1:300] == t1))
  inds   <- ev$sampleIndex[-1]
  to_rem <- c(1:h_len, (inds-1) * n_chan + h_len + 1:2)

  raw_val    <- matrix(val_i[-to_rem] / 2^30,
                   ncol = n_chan,
                   byrow = TRUE)



  # get datetime values (originally milliseconds)
  dat <- data.table(
    serial = im$serialID,
    datetime = rbr_times(as.numeric(ev$tstamp * 1e-3),
                                         as.integer(ev$sampleIndex),
                                         ti))


  # keep raw values
  if ('raw' %in% type) {
    for(i in seq_along(ch_name)) {
      dat[, paste0(ch_name[[i]], '_raw') := raw_val[, i]]
    }
  }



  # do basic calibration (C coefficients)
  if ('basic' %in% type | 'compensated' %in% type) {
    # prepare temperature for calibration equation
    is_temp <- 'Temperature' == ch$longName

    for(i in seq_along(ch_name)) {

      if(is_temp[i]){
        dat[, ch_name[[i]] := rbr_raw_to_temperature(raw_val[, i], cs[i,])]
      } else {
        dat[, ch_name[[i]] := rbr_raw_to_pressure(raw_val[, i], cs[i,])]
      }

    }
  }






  # do temperature compensation
  if (length(pressure_index) > 0 & length(temperature_index) > 0) {

    if ('compensated' %in% type) {

      dat[, pressure_dbar_comp := rbr_temperature_correction(
                                     get(ch_name[[pressure_index]]),
                                     get(ch_name[[temperature_index]]),
                                     x0)]

    }
  }



  setkey(dat, datetime)
  dat <- data.table::melt(dat, id.vars = c('serial','datetime'))

  invisible(dat)

}



#===============================================================================
.get_correction_coef <- function(co) {
  as.numeric(co[grepl('x', key)]$value)
}



#===============================================================================
.get_base_coef <- function(co) {
  co  <- co[grepl('c', key)]
  co  <- co[, value := as.numeric(value)]
  co  <- data.table::dcast(co, calibrationID~key, value.var = 'value')
  co  <- as.matrix(co[, calibrationID := NULL])
}

# .rbr_times <- function(ev, ti) {
#
#   ep  <- ev$tstamp / 1000
#   ind <- ev$sampleIndex
#
#   dt <- vector(mode = "list", length(ep) - 1)
#
#   for(i in seq_along(dt)) {
#     dif <- (ind[i+1] - ind[i]) - (i)
#     dt[[i]] <- ep[i] + 0:(dif) * ti
#   }
#
#   as.POSIXct(unlist(dt), tz = 'UTC', origin = '1970-01-01')
#
#
# }



#===============================================================================
.rbr_parse <- function(h) {

  # vector of values
  readBin(h,
          n = 100000L,
          what = 'integer',
          size = 4L,
          signed = TRUE,
          endian = 'little')

}


# library(transducer)
# fn <- list.files('/home/jonathankennel/Downloads/', pattern = 'rsk', full.names = TRUE)
# x <- fn[8]
# x <- '/home/jonathankennel/Storage/data/rbr/rd130 077623_20191119_1500.rsk'
# x <- '/home/jonathankennel/Storage/data/rbr/rd45a 081871_20191118_1213.rsk'
#
#
# system.time(a <- read_rbr_raw(x))
# (b <- read_rbr(x))
# # system.time(a <- read_rbr_tmp(x))
