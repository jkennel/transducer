#' #===============================================================================
#' #' read_rbr_bin
#' #'
#' #' @param bin_dat
#' #' @param cal
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #===============================================================================
#' read_rbr_bin <- function(bin_dat,
#'                          cal,
#'                          n_channels = 1,
#'                          time_interval) {
#'
#'   # https://docs.rbr-global.com/L3commandreference/format-of-stored-data/standard-rawbin00-format/deployment-header/version-2-002
#'   h  <- bin_dat[[1]]
#'   nb <- length(bin_dat)
#'   nl <- length(bin_dat[[nb]]) / 4L
#'
#'   # read an initial portion of data
#'   h_s <- readBin(h,
#'                  n = 800L,
#'                  what = 'integer',
#'                  size = 4L,
#'                  signed = TRUE,
#'                  endian = 'little')
#'
#'   # indices
#'   # ts <- 1:4 + header_size + 4 # starting time
#'   # ti <- 1:4 + 24              # time interval
#'   ts <- max(which(h_s < -134217728)) * 4 + 1
#'   ts <- ts:(ts + 3)
#'   si <- max(ts) + 1           # sample start
#'   n  <- length(h)             # end
#'
#'
#'   # starting time
#'   time_start    <- as.POSIXct(
#'
#'     readBin(h[ts],
#'             n = 1L,
#'             what = 'integer',
#'             size = 4L,
#'             signed = TRUE,
#'             endian = 'little'),
#'     tz = 'UTC',
#'     origin = '2000-01-01'
#'
#'   )
#'
#'   # # interval time
#'   # time_interval <- readBin(h[ti],
#'   #                          n = 1L,
#'   #                          what = 'integer',
#'   #                          size = 4L,
#'   #                          signed = TRUE,
#'   #                          endian = 'little') / 1000
#'
#'
#'
#'   val_1 <- readBin(h[si:n],
#'                    n = 100000L,
#'                    what = 'integer',
#'                    size = 4L,
#'                    signed = TRUE,
#'                    endian = 'little')
#'
#'   val_n <- readBin(bin_dat[[nb]],
#'                    n = nl-2,
#'                    what = 'integer',
#'                    size = 4L,
#'                    signed = TRUE,
#'                    endian = 'little')
#'
#'   x <- unlist(c(val_1,
#'                 lapply(bin_dat[-c(1, nb)], readBin, n = 1000000L,
#'                        what = 'integer',
#'                        size = 4L,
#'                        signed = TRUE,
#'                        endian = 'little'),
#'                 val_n))/2^30
#'
#'
#'   dat <- data.table(datetime = time_start + 0:(length(x)-1) * time_interval,
#'                     value = cal[1] + cal[2]*x + cal[3]*x^2 + cal[4]*x^3,
#'                     # value_cpp = calib_mult(x, cal),
#'                     mv = x
#'   )
#'
#'   setkey(dat, datetime)
#'
#' }
#'
#'
#'
#' #===============================================================================
#' #' read_rbr_tmp
#' #'
#' #' @param x
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #===============================================================================
#' read_rbr_tmp <- function(x) {
#'
#'   # https://docs.rbr-global.com/L3commandreference/format-of-stored-data/standard-rawbin00-format/standard-format-events-markers
#'   # Seconds for the date/time are counted from 2000-01-01T00:00:00Z.
#'
#'   # setup database connection
#'   db <- RSQLite::dbConnect(RSQLite::SQLite(), x)
#'   # RSQLite::dbListTables(conn = db)
#'
#'   # system.time(dat <- RSQLite::dbGetQuery(db, "SELECT * FROM data"))
#'   # ccc <- RSQLite::dbGetQuery(db, "SELECT * FROM calibrations")
#'   # ccc <- RSQLite::dbGetQuery(db, "SELECT * FROM parameters")
#'
#'   # continuous table (time interval)
#'   ti <- RSQLite::dbGetQuery(db, "SELECT samplingPeriod FROM continuous")[[1]]/1000
#'
#'   # epochs table (start time)
#'   ep <- RSQLite::dbGetQuery(db, "SELECT startTime/1000 FROM epochs")[[1]]
#'   ep <- as.POSIXct(ep, origin = '1970-01-01', tz = 'UTC')
#'
#'   # instruments table
#'   im <- RSQLite::dbGetQuery(db, "SELECT * FROM instruments")
#'
#'   # coefficient table
#'   co <- data.table::as.data.table(RSQLite::dbGetQuery(db, "SELECT * FROM coefficients"))
#'
#'   # channel table
#'   ch <- RSQLite::dbGetQuery(db, "SELECT * FROM channels")
#'
#'   # errors table
#'   er <- RSQLite::dbGetQuery(db, "SELECT * FROM errors")
#'
#'   # events table
#'   ev <- RSQLite::dbGetQuery(db, "SELECT * FROM events")
#'
#'   # raw table
#'   dl <- RSQLite::dbGetQuery(db, "SELECT data FROM downloads")[[1]]
#'
#'   RSQLite::dbDisconnect(db)
#'
#'
#'   x0 <- .get_correction_coef(co)
#'   cs <- .get_base_coef(co)
#'
#'
#'   n_chan <- nrow(ch)
#'   n_blob <- length(dl)
#'   ch_name <- ch$shortName
#'
#'
#'   # read binary data
#'   val_i <- unlist(lapply(dl, .rbr_parse))
#'
#'   # remove header
#'   t1     <- as.numeric(ev$tstamp)[1] * 1e-3 - as.numeric(as.POSIXct('2000-01-01 00:00:00', tz = 'UTC'))
#'   h_len  <- max(which(val_i[1:300] == t1))
#'   inds   <- ev$sampleIndex[-1]
#'   to_rem <- c(1:h_len, (inds-1) * n_chan + h_len + 1:2)
#'
#'   val <- matrix(val_i[-to_rem] / 2^30,
#'                 ncol = n_chan,
#'                 byrow = TRUE,
#'                 dimnames = list(list(), paste0(ch_name, "_raw")))
#'
#'   datetime <- rbr_times(ev$tstamp/1000,
#'                         ev$sampleIndex,
#'                         ti)
#'
#'
#'   # prepare temperature for calibration equation
#'   is_temp <- 'Temperature' == ch$longName
#'
#'
#'   # apply basic calibration equation
#'   simp_cal <-  rbr_calib_mult(val, cs, is_temp)
#'
#'   # temperature correction for pressure
#'   pressure_index    <- as.numeric(co[grepl('n0', key)]$calibrationID)
#'   temperature_index <- as.numeric(co[grepl('n0', key)]$value)
#'
#'
#'   # create data.table.  surprisingly this seems to be faster than as.data.table
#'   out <- data.table(datetime)
#'   for(i in seq_along(ch_name)) {
#'     out[, paste0(ch_name[i], '_raw') := val[, i]]
#'     out[, ch_name[i] := simp_cal[, i]]
#'   }
#'
#'
#'   if (length(pressure_index) > 0 & length(temperature_index) > 0) {
#'     out[, pressure_dbar_comp := rbr_temperature_correction(get(ch_name[pressure_index]),
#'                                                            get(ch_name[temperature_index]),
#'                                                            x0)]
#'
#'
#'
#'
#'   }
#'
#'   setkey(out, datetime)
#'   invisible(out)
#'
#' }
#'
#'
#'
#'
#'
#'
#' # .rbr_parse_n <- function(h) {
#' #
#' #   # get values
#' #   val <- .rbr_parse(h)
#' #
#' #   # remove last two values
#' #   nl <-  max(which(val < -134217728)) -1
#' #
#' #   # subset
#' #   val[1:nl]
#' #
#' # }
#'
#'
#' # library(data.table)
#' # library(transducer)
#' # fn <- list.files('/home/jonathankennel/Downloads/', pattern = 'rsk', full.names = TRUE)
#' #
#' # x <- fn[8]
#' # system.time(a <- read_rbr_tmp(x))
#' # system.time(a <- rbr_read_raw(x, c('raw', 'basic', 'compensated')))
#' # system.time(b <- read_rbr(x))
#' # x <- '/home/jonathankennel/Storage/data/rbr/rd130 077623_20191119_1500.rsk'
#' # system.time(a <- read_rbr_tmp(x))
#' # system.time(b <- rbr_read_raw(x, c('raw')))
#' # x <- '/home/jonathankennel/Storage/data/rbr/rd45a 081871_20191118_1213.rsk'
#' # a <- read_rbr_tmp(x)
#' # system.time(a <- read_rbr_tmp(x))
#' # system.time(a <- rbr_read_raw(x, c('raw', 'compensated')))
#' # # system.time(b <- read_rbr(x))
#' # x <- fn[6]
#' # system.time(a <- read_rbr_tmp(x))
#' # system.time(a <- read_rbr(x))
#'
#'
#' #
#' #
#' #
#' # rbr_glance(fn[15])
#' #
#' # system.time(dt <- .rbr_times(ev, ti))
#' # table(diff(dt))
#' #
#' #
#' # all.equal(dat$datetime, .rbr_times(ev, ti), tolerance = 1e-12)
#' #
#' # tail(dat)
#' #
#' # dat[, datetime := as.POSIXct(tstamp/1000, tz = 'UTC', origin = '1970-01-01')]
#' # dat_dt <- (dat$tstamp/1000)
#' # dt_dt
#' #
#' # all.equal(tail(dat_dt, 5), tail(dt_dt, 5), tolerance = 1e-12)
#' #
#' # all.equal(dat_dt, dt_dt[-length(dt_dt)], tolerance = 1e-14)
#' #
#' #
#' #
#' #
#' #
#' #
#' #
#' #
#' # # system.time(
#' # #   td <- read_transducer('/home/jonathankennel/Storage/data/rbr/rd130 077623_20191119_1500.rsk')
#' # # )
#' # #
#' # system.time({
#' #
#' #   con <- dbConnect(RSQLite::SQLite(), '/home/jonathankennel/Storage/data/rbr/rd130 077623_20191119_1500.rsk')
#' #
#' #   tbl_name <- 'coefficients'
#' #   rs <- dbSendQuery(con, paste0("SELECT * FROM ", tbl_name))
#' #   cal <- as.numeric(dbFetch(rs)$value)
#' #
#' #   tbl_name <- 'downloads'
#' #   rs <- dbSendQuery(con, paste0("SELECT * FROM ", tbl_name))
#' #   dl <- dbFetch(rs)
#' #
#' #   dat <- read_rbr_bin(dl$data, cal)
#' # })
#' # # all.equal(dat[, list(datetime, value)], td$data[[1]])
#' # −134217728
#' # -150881402
#' # -150886185
#'
#' # microbenchmark::microbenchmark(
#' #   data.table(m),
#' #   as.data.frame(m),
#' #   times = 20
#' # )
#'
