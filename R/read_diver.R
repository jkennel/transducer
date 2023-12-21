#===============================================================================
#' @title obtain data from diver .mon file
#'
#' @description import diver data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param x character the path to the rbr database ( rsk )
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_diver <- function (x, ...) {
  UseMethod("read_diver", x)
}




#' read_diver
#'
#' @param db_name character the path to the rbr database ( rsk )
#'
#' @return data.table of results
#' @export
#'
#' @import data.table
#'
#' @examples
read_diver.character <- function(x, ...) {


  # check for issue with the files
  .check_files(x)


  # check if it is a binary file (*.DAT)
  if(grepl(tools::file_ext(x), '.dat', ignore.case = TRUE)) {
    warning('Binary file.  This feature is experimental. For improvements the Diver binary format should be made public.')
    class(x) <- c('binary', class(x))
  } else {
    class(x) <- c('ascii', class(x))
  }


  read_diver(x)

}

#' read_diver
#'
#' @param db_name
#' @param head_size
#'
#' @return data.table of results
#' @export
#'
#' @import data.table
#'
#' @examples
read_diver.ascii <- function(x,
                             transducer_depth = NULL,
                             well_elevation   = NULL,
                             ...) {


  h <- readLines(x, 100)
  h <- iconv(h, "UTF-8", "UTF-8",sub='')
  h <- gsub("\t", " ", h)  # replace tabs with spaces


  if(trimws(h[1]) != 'Data file for DataLogger.'){
    warning(paste(x, 'is not a standard MON file'))
    return(NULL)
  }

  # h       <- gsub('\xb0', '', h)
  # h       <- gsub('\xba', '', h)
  h       <- tolower(h)
  h       <- gsub('instrument info from data header', 'series settings', h)
  h       <- gsub('instrument info', 'logger settings', h)


  channel <- .parse_diver_channels(h)
  channel[parameter == 'unit', unit := value]

  logger  <- .parse_logger_setting(h)
  series  <- .parse_series_setting(h)
  info    <- unique(rbindlist(list(logger, series)))
  info    <- info[parameter != '']
  chan    <- channel[parameter == 'identification']$value

  if(is.null(chan)) {
    return(NULL)
  }

  data    <- .parse_diver_data(x, h, chan)
  n       <- nrow(data)

  if(all(is.na(data$datetime))) {
    pred_datetime <- as.POSIXct(as.numeric(info[parameter == 'start_date_time']$value) +
                                  0:(n-1) * as.numeric(info[parameter == 'sample_rate']$value),
                                origin = '1970-01-01',
                                tz = 'UTC')

    data[, datetime := pred_datetime]
  }


  if (nrow(data) == 1) {
    warning('Only one measurement in dataset')
    dt <- NA_real_
  } else {
    dt      <- unique(diff(as.numeric(data$datetime)))
  }

  tz      <- .parse_diver_timezone(h)

  if(!is.na(tz)) {
    attributes(data$datetime)$tzone <- tz
  }

  start   <- min(data$datetime)
  end     <- max(data$datetime)

  data <- melt(data, id.vars = 'datetime')
  data <- lapply(chan, function(x) data[variable == x, ][, variable := NULL])

  channel <- split(channel, channel$channel)
  units   <- .get_diver_units(channel)


  version <- tail(strsplit(h[8], ':')[[1]], 1)

  # setkey(data, datetime)
  dat <- data.table(file = x,
             # info = list(info),
             channel = chan,
             data = data,
             id = NA_character_,
             calibration = list(data.table(coef = character(), value = numeric())),
             parameter = chan,
             units = units,
             version = version,
             serial = substr(info[parameter == 'serial_number']$value, 6, 10),
             model = info[parameter == 'instrument_type']$value,
             dt = dt
             # n = n
             )

  if(!is.null(well_elevation)) {
    dat[, well_elevation := well_elevation]
  }

  if(!is.null(transducer_depth)) {
    dat[, transducer_depth := transducer_depth]
  }

  dat
  # setcolorder(h, "file", "channel", "data", "id", "calibration",
  #                "parameter", "units", "version", "serial", "model", "dt")




}





#' read_diver
#'
#' @param x
#' @param head_size
#'
#' @return data.table of results
#' @export
#'
#' @import data.table
#'
#' @examples
read_diver.binary <- function(x, head_size = 280, ...) {

  # read raw binary
  b_dat <- readBin(x,
                   n = 1000000L,
                   what = 'raw')

  # temperature and pressure file
  val <- readBin(b_dat[-(1:head_size)],
                 n = 1000000L,
                 what = 'integer',
                 size = 2L,
                 endian = 'little')

  val_adj <- (val %/% 256) * 6
  scale_adj <- 0

  val <- (val - val_adj) / 30000

  h   <- .parse_diver_header_binary(b_dat[1:head_size])

  pressure    <- (h$min[1] + h$rng[1] * val[seq(1, length(val), 2)])
  temperature <- (h$min[2] + h$rng[2] * val[seq(2, length(val), 2)])

  h   <- h[, data := list(data.table(datetime = h$start[1] + 0:(length(pressure)-1) * h$dt[1],
                                     value = pressure),
                          data.table(datetime = h$start[1] + 0:(length(pressure)-1) * h$dt[1],
                                     value = temperature))]
  h[, file := as.character(x)]
  h[, n := nrow(data[[1]])]
  h[, c("file", "channel", "data", "id", "calibration",
        "parameter", "units", "version", "serial", "model", "dt"), with = FALSE]





}



.get_diver_units <- function(x) {

  sapply(x, function(z) na.omit(z$unit)[1])

}



.parse_diver_header_binary <- function(x) {


  id     <- trimws(rawToChar(x[4:23]))
  serial <- trimws(rawToChar(x[29:33]))
  tz     <- trimws(rawToChar(x[50:54]))


  param1 <- trimws(rawToChar(x[56:68]))
  param2 <- trimws(rawToChar(x[144:156]))


  min1   <- as.numeric(trimws(rawToChar(x[78:85])))
  min2   <- as.numeric(trimws(rawToChar(x[160:170])))


  rng1   <- as.numeric(trimws(rawToChar(x[95:101])))
  rng2   <- as.numeric(trimws(rawToChar(x[179:186])))


  units1 <- trimws(rawToChar(x[88:95]))
  units2 <- trimws(rawToChar(x[173:178]))


  dt     <- as.numeric(strptime(paste('1970-01-01', trimws(rawToChar(x[230:238]))),
                                format = '%Y-%m-%d %X', tz = 'UTC'))
  start  <- as.POSIXct(trimws(rawToChar(x[244:261])), format = '%S:%M:%H %d/%m/%y', tz = 'UTC')
  end    <- as.POSIXct(trimws(rawToChar(x[263:280])), format = '%S:%M:%H %d/%m/%y', tz = 'UTC')

  h <- rbind(data.table(
    # id      = tolower(id),
    serial  = tolower(serial),
    tz      = tz,
    id      = NA_character_,
    channel = tolower(param1),
    parameter = tolower(param1),
    min     = min1,
    rng     = rng1,
    units   = tolower(units1),
    dt      = dt,
    start   = start,
    end     = end,
    version = NA_character_,
    model = NA_character_,
    calibration = list(data.table(coef = character(), value = numeric()))
  ), data.table(
    # id      = tolower(id),
    serial  = tolower(serial),
    tz      = tz,
    id      = NA_character_,
    channel = tolower(param2),
    parameter = tolower(param2),
    min     = min2,
    rng     = rng2,
    units   = tolower(units2),
    dt      = dt,
    start   = start,
    end     = end,
    version = NA_character_,
    model = NA_character_,
    calibration = list(data.table(coef = character(), value = numeric()))
  ))

}



.parse_diver_timezone <- function(h) {

  tzs <- grep('UTC', h)
  num <- unique(gsub("[^0-9.-]", "", trimws(h[tzs]), perl = TRUE))

  if(length(num) != 1) {
    return(NA_character_)
  }

  if(is.na(as.numeric(num))) {
    return(NA_character_)
  }

  tzs <- paste0('UTC', num)


}



.parse_diver_row <- function(x) {

  x <- trimws(x)
  x <- strsplit(x, '=', fixed = TRUE, useBytes = TRUE)[[1]]
  x <- trimws(x)
  x <- tolower(x)
  x[1] <- gsub('/', ' ', x[1])
  x <- gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)
  x <- gsub("water head \\(toc-cl\\)", 'water_head_toc_cl', x, ignore.case = TRUE)
  x <- gsub("water head \\(toc-mm\\)", 'water_head_toc_mm', x, ignore.case = TRUE)
  x <- gsub("deg c", 'c', x, ignore.case = TRUE)
  x <- gsub("m asl", 'masl', x, ignore.case = TRUE)
  x <- gsub("ft asl", 'ftasl', x, ignore.case = TRUE)

  x[1] <- gsub(' ', '_', x[1])

  if(length(x) == 1) {
    x <- data.table(parameter = x)
    return(x)
  }

  x <- c(x[1], strsplit(x[2], ' ', fixed = TRUE, useBytes = TRUE)[[1]])
  x <- data.table(t(x))

  if (ncol(x) == 2) {
    setnames(x, c('parameter', 'value'))
  } else if (ncol(x) == 3) {
    setnames(x, c('parameter', 'value', 'unit'))
  }

  x
}


.parse_diver_row_settings <- function(x) {

  x <- strsplit(x, '=', fixed = TRUE, useBytes = TRUE)[[1]]
  x <- trimws(x)
  x <- tolower(x)
  x[1] <- gsub('/', ' ', x[1])
  x <- gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)
  x[1] <- gsub(' ', '_', x[1])
  x <- c(x[1], paste(x[-1], collapse = '='))
  x <- data.table(t(x))

  setnames(x, c('parameter', 'value'))

  x
}



.parse_logger_setting <- function(h) {


  logger_ind <- grep('[logger settings]', h, fixed = TRUE, useBytes = TRUE)
  breaks <- grep('[', h, fixed = TRUE, useBytes = TRUE)
  s <- data.table(start = logger_ind + 1)
  e <- data.table(start = breaks-1)
  s[e, end := i.start, roll = 10, on = 'start']

  sub   <- s$start:s$end
  h_sub <- h[sub]

  x <- rbindlist(lapply(h_sub, .parse_diver_row_settings))

  x[parameter %in% 'sample_period', parameter := 'sample_rate']

  x[parameter == 'sample_rate', value := .parse_diver_period_logger(value)]

  x

}



.parse_series_setting <- function(h) {


  series_ind <- grep('[series settings]', h, fixed = TRUE, useBytes = TRUE)
  breaks <- grep('[', h, fixed = TRUE, useBytes = TRUE)
  s <- data.table(start = series_ind + 1)
  e <- data.table(start = breaks-1)
  s[e, end := i.start, roll = 10, on = 'start']

  sub   <- s$start:s$end
  h_sub <- h[sub]

  x <- rbindlist(lapply(h_sub, .parse_diver_row_settings))

  x[parameter %in% c('tb', 'start_time'), parameter := 'start_date_time']
  x[parameter %in% c('te', 'end_time', 'stop_time'), parameter := 'end_date_time']
  x[parameter %in% 'sample_period', parameter := 'sample_rate']


  x[parameter == 'start_date_time', value := as.POSIXct(value, format = '%S:%M:%H %d/%m/%y')]
  x[parameter == 'end_date_time', value := as.POSIXct(value, format = '%S:%M:%H %d/%m/%y')]

  x[parameter == 'sample_rate',   value := .parse_diver_period_series(value)]

  x


}


.parse_diver_channels <- function(h) {

  inds     <- grep('=', h, fixed = TRUE, useBytes = TRUE)
  inds_rem <- grep('==', h, fixed = TRUE, useBytes = TRUE)
  inds     <- inds[!inds %in% inds_rem]


  breaks <- grep('[', h, fixed = TRUE, useBytes = TRUE)
  channels_ind <- grep('.channel.*from data header', h)
  breaks <- breaks[breaks > min(channels_ind)]
  s <- data.table(end = channels_ind + 1)
  e <- data.table(end = breaks-1)
  s <- na.omit(e[s, start := end, roll = -Inf, on = 'end'])
  channels <- gsub("[^0-9.-]", "", h[channels_ind])


  channel_dat <- list()
  for(i in 1:nrow(s)) {
    sub <- s[i]$start:s[i]$end
    h_sub <- h[sub]
    h_sub <- h_sub[h_sub != '']

    channel_dat[[i]] <- rbindlist(lapply(h_sub, .parse_diver_row), fill = TRUE)
    channel_dat[[i]][, channel := channels[i]]
  }

  channel_dat <- unique(rbindlist(channel_dat, fill = TRUE))

}


.parse_diver_period_logger <- function(x) {

  if (!is.na(suppressWarnings(as.numeric(x)))){
    return(as.numeric(x) / 100)
  }

    char <- substring(x, 1, 1)
    num  <- as.numeric(substring(x, 2))

    if (char == 'm') {
      num <- num * 60
    } else if (char == 's') {
      num <- num
    } else if (char == 't') {
      num <- num/100
    }
    return (num)

}



.parse_diver_period_series <- function(x) {

  if (!is.na(suppressWarnings(as.numeric(x)))){
    return(as.numeric(x) / 100)
  }

  x <- strsplit(x, ' ')
  d <- sprintf("%02d", as.numeric(x[[1]][1]) + 1)
  hms <- x[[1]][2]
  ss <- paste0('.', x[[1]][3])

  num <- as.numeric(as.POSIXct(paste0('1970-01-', d,' ', hms, ss),
                                   format = '%Y-%m-%d %H:%M:%OS', tz = 'UTC'))

  return (num)

}



.parse_diver_data <- function(db_name, h, channels) {

  d <- grep('[data]', h, fixed = TRUE, useBytes = TRUE)
  n <- as.numeric(h[d+1])

  if(n == 0) {
    return(data.table(1)[,(channels) := NA][,V1:=NULL][.0])
  }

  dat <- data.table::fread(db_name, nrows = n,
                           skip = d+1,
                           header = FALSE,
                           fill = TRUE,
                           sep2 = '\t')


  if (ncol(dat) == 3) {

    setnames(dat, c('datetime', channels))

    dat[, datetime := as.POSIXct(NA_real_)]

    return(dat)
  }

  setnames(dat, c('date', 'time', channels))

  dat[, datetime := as.POSIXct(paste(date, time),
                               format = '%Y/%m/%d %H:%M:%OS', tz = 'UTC')]
  dat[, date := NULL]
  dat[, time := NULL]

  dat

}
