#' read_levelogger
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_levelogger <- function(x,
                            transducer_depth = NULL,
                            well_elevation   = NULL,
                            ...) {

  r <- XML::xmlRoot(XML::xmlParse(x, encoding = "ISO-8859-1"))

  download_time <- .parse_datetime_levelogger(
    XML::getChildrenStrings(r[['File_info']][['Date']]),
    XML::getChildrenStrings(r[['File_info']][['Time']]))

  version_s <- XML::getChildrenStrings(r[['File_info']][['Created_by']])
  version_f <- XML::getChildrenStrings(r[['Instrument_info']][['Firmware']])

  instrument <- XML::getChildrenStrings(r[['Instrument_info']])
  header     <- XML::getChildrenStrings(r[['Instrument_info_data_header']])

  n_channel  <- as.numeric(instrument[['Channel']])


  channels <- list()
  for(i in 1:n_channel) {
    channels[[i]] <- data.table(t(
      tolower(XML::getChildrenStrings(r[[paste0('Ch', i, '_data_header')]]))))
  }
  channels <- rbindlist(channels)

  len_dat <- 3 + n_channel
  nms  <- as.character(names(r[['Data']][['Log']]))
  data <- vapply(XML::xmlChildren(r[['Data']]),
                           function(x) XML::getChildrenStrings(x, asVector = TRUE, len = len_dat), FUN.VALUE = rep(NA_character_, len_dat))
  data <- as.data.table(matrix(unlist(data),
                               ncol = len_dat,
                               byrow = TRUE,
                               dimnames = list(NULL, nms)))

  cols <- names(data)[!names(data) %chin% c('Date', 'Time')]
  data[, (cols):= lapply(.SD, as.numeric), .SDcols = cols]

  data[, datetime := .parse_datetime_levelogger(Date, Time) + as.numeric(ms)/1000]

  start <- min(data$datetime)
  end   <- max(data$datetime)

  data[, Date := NULL]
  data[, Time := NULL]
  data[, ms := NULL]
  setnames(data, c(channels$Identification, 'datetime'))

  data <- melt(data, id.vars = 'datetime')
  data <- lapply(channels$Identification, function(x) data[variable == x, ][, variable := NULL])


  dat <- data.table(
    file    = x,
    channel = channels$Identification,
    data    = data,
    id      = header['Event_ch'],
    calibration = list(data.table(coef = character(), value = numeric())),
    parameter =  channels$Identification,
    units = channels$Unit,
    version = instrument['Firmware'],
    serial = instrument['Serial_number'],
    model = paste(instrument['Instrument_type'], instrument['Model_number']),
    dt = as.numeric(header['Sample_rate']) * 10
    # n = as.numeric(header['Num_log'])
  )

  if(!is.null(well_elevation)) {
    dat[, well_elevation := well_elevation]
  }

  if(!is.null(transducer_depth)) {
    dat[, transducer_depth := transducer_depth]
  }

  # setcolorder(h, "file", "channel", "data", "id", "calibration",
  #                "parameter", "units", "version", "serial", "model", "dt")

  dat
}


.parse_datetime_levelogger <- function(x, y) {

  as.POSIXct(paste0(x, y), format = '%Y/%m/%d %H:%M:%S', tz = 'UTC')

}
