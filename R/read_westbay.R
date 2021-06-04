#===============================================================================
#' read_westbay
#'
#' @param x name of the file
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#===============================================================================
read_westbay <- function (x,
                          transducer_depth = NULL,
                          well_elevation   = NULL,
                          ...) {
  UseMethod("read_westbay", x)
}

#===============================================================================
#' read_westbay.character
#'
#' @param x name of the file
#' @param tranducer_depth
#' @param well_elevation
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' #===============================================================================
read_westbay.character <- function(x,
                                   transducer_depth = NULL,
                                   well_elevation   = NULL,
                                   ...) {

  .check_files(x)


  h      <- .get_westbay_header(x)
  elev   <- .get_westbay_elevation(h)
  serial <- .get_westbay_probe_serial(h)
  d      <- .get_westbay_data(x)
  dat    <- .parse_westbay_data(d)
  dat    <- dat[, .split_parameters_westbay(data), by = probe_id]
  depths <- .get_westbay_transducer_depths(d)

  dat    <- dat[depths, on = 'probe_id', nomatch = 0]
  dat    <- dat[serial, on = 'probe_id']

  dat[, parameter := channel]
  dat[, model := NA_character_]
  dat[, version := NA_character_]
  dat[, id := probe_id]
  dat[, file := x]
  dat[, dt := unique(diff(as.numeric(data[[1]]$datetime[1:2]))), by = list(channel, port)]
  dat[, well_elevation := .get_westbay_elevation(h)[['elev']]]

  if (!is.null(transducer_depth)) {
    dat[, transducer_depth := transducer_depth]
  }

  if (!is.null(well_elevation)) {
    dat[, well_elevation := well_elevation]
  }

  dat[, units := .get_westbay_elevation(h)[['units']]]
  dat[, calibration := list(data.table(coef = character(), value = numeric()))]

  setcolorder(dat, c("file", "channel", "data", "id", "calibration",
                     "parameter", "units", "version", "serial", "model", "dt",
                     "transducer_depth", "well_elevation"))

}


#===============================================================================
.split_parameters_westbay <- function(x) {

  x <- split(x[[1]], x[[1]]$variable)
  data.table(data = x, channel = names(x))

}

#===============================================================================
.get_westbay_header <- function(x) {

  h <- readLines(x, warn = FALSE)

  wh <- which(grepl('[Data]', h, fixed = TRUE))

  h <- h[1:wh]

  return(h)

}


#===============================================================================
.get_westbay_data <- function(x) {

  ind  <- readLines(x, warn = FALSE)
  wh   <- grep('[Data]', ind, fixed = TRUE)

  dat <- fread(x, skip = wh+1,
               na.strings = c("", "NA", "N A", "N / A", "N/A", "N/ A",
                              "Not Available", "NOt available",
                              '"n/a"', 'n/a'))

  setnames(dat, c('name', 'datetime', 'pressure', 'temperature',
                  'probe_id', 'probe_status', 'probe_description',
                  'port', 'port_description', 'depth', 'comments'))


  dat[, pressure := as.numeric(pressure)]
  dat[, temperature := as.numeric(temperature)]
  dat[, transducer_depth := as.numeric(depth)]
  dat[, comments := as.character(comments)]
  dat[comments == '', comments := NA_character_]
  dat[, probe_description := as.character(probe_description)]
  dat[probe_description == '', probe_description := probe_description]
  dat[, datetime := as.POSIXct(datetime, format = '%Y/%m/%d %H:%M:%S', tz = 'EST')]

  setkey(dat, datetime)

  dat

}

#===============================================================================
.parse_westbay_data <- function(x) {

  x <- x[, melt(.SD, id.vars = c('datetime', 'probe_status')),
         by = probe_id,
         .SDcols = c('datetime', 'pressure', 'temperature', 'probe_status')]
  setkey(x, probe_id)

  x <- split(x, x$probe_id)
  x <- lapply(x, function(x) x[, probe_id := NULL])
  x <- data.table(data = x,
                  probe_id = as.integer(names(x)))
  x


}


#===============================================================================
.get_westbay_probe_serial <- function(x) {

  wh <- which(grepl('Logical Probe-', x, fixed = TRUE))
  matches <- regmatches(x[wh], gregexpr("[[:digit:]]+", x[wh],))

  probe_id     <- as.integer(sapply(matches, '[', 1))
  serial <- sapply(matches, '[', 2)

  data.table(probe_id, serial)

}


#===============================================================================
.get_westbay_transducer_depths <- function(x) {

  unique(x[, list(probe_id,
                  port,
                  probe_description,
                  port_description,
                  transducer_depth = depth)])


}



# .read_westbay_db <- function(x) {
#
#   h <- parse_westbay_header(x)
#   h[, file := x]
#   h[, file_name := basename(x)]
#
#   dat <- .parse_westbay_data(x, h$n_col, attr(h$download_time, 'tz'))
#   dat <- dat[is.na(comments)]
#   dt <- dat[, list(dt = diff(as.numeric(datetime))), by = port]
#   dt <- as.numeric(names(which.max(table(dt$dt))))
#
#   h[, dt := dt]
#   h[, start := min(dat$datetime)]
#   h[, end := max(dat$datetime)]
#
#   h[, data := .(dat)]
#   h[, n := nrow(dat)]
#
#   setkey(h, start)
#
#   h
# }


# .parse_westbay_data <- function(x, n_col, tz) {
#
#   ind  <- readLines(x, warn = FALSE)
#   wh   <- grep('[Data]', ind, fixed = TRUE)
#
#   dat <- fread(x, skip = wh+1,
#                na.strings = c("", "NA", "N A", "N / A", "N/A", "N/ A",
#                               "Not Available", "NOt available",
#                               '"n/a"', 'n/a'))
#
#   setnames(dat, c('name', 'datetime', 'pressure', 'temperature',
#                   'probe_id', 'probe_status', 'probe_description',
#                   'port', 'port_description', 'depth', 'comments'))
#
#   setkey(dat, datetime)
#
#   dat[, pressure := as.numeric(pressure)]
#   dat[, temperature := as.numeric(temperature)]
#   dat[, depth := as.numeric(depth)]
#   dat[, comments := as.character(comments)]
#   dat[comments == '', comments := NA_character_]
#   dat[, probe_description := as.character(probe_description)]
#   dat[probe_description == '', probe_description := probe_description]
#   dat[, datetime := as.POSIXct(datetime, format = '%Y/%m/%d %H:%M:%S', tz = tz)]
#
#   probe_dat <- unique(dat[, list(probe_id, port, probe_description, port_description, depth)])
#
#   data <- dat[, melt(.SD, id.vars = c('datetime', 'probe_status')),
#                 by = probe_id,
#                 .SDcols = c('datetime', 'pressure', 'temperature', 'probe_status')]
#   setkey(data, probe_id)
#
#   data <- split(data, data$probe_id)
#   data <- lapply(data, function(x) x[,probe_id := NULL])
#   data <- data.table(data = data,
#              probe_id = names(data),
#              probe_dat = list(probe_dat))
#
#
#   invisible(data)
# }
