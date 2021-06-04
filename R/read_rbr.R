#' read_rbr
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_rbr <- function (x, ...) {
  UseMethod("read_rbr", x)
}


#' read_rbr
#'
#' @param db_name character the path to the rbr database ( rsk )
#' @param start starting date to get
#' @param end ending date to get
#' @param by spacing
#' @param use_rbr_tz boolean use time zone information from the rbr file?
#'
#' @return data.table of results
#' @export
#'
#' @import data.table
#'
#' @examples
#' db_name <- system.file("extdata", "example.rsk", package="transducer")
#' system.time(wl <- read_rbr(db_name))
#' system.time(wl <- read_rbr(db_name, by = 3600))
#' system.time(wl <- read_rbr(db_name, by = 1800))
#' system.time(wl <- read_rbr(db_name, by = 900))
#' system.time(wl <- read_rbr(db_name, by = 600))
#' system.time(wl <- read_rbr(db_name, by = 300))
#' system.time(wl <- read_rbr(db_name, by = 10))
#'
#' db_name <- '/media/kennel/Data/phd/personnel/pat/syngenta/SYN UW1 P1-07 - 077924_20150413_1746.rsk'
#' system.time(wl <- read_rbr(db_name))
#' system.time(wl <- read_rbr(db_name,
#'  start = as.POSIXct('2015-01-01', tz = 'UTC'),
#'  end = as.POSIXct('2015-02-01', tz = 'UTC'),
#'  by = 60))
#'
#' db_name <- '/media/kennel/Data/tmp/rd10 077611_20181108_1037.rsk'
#' system.time(wl <- read_rbr(db_name))
#'
read_rbr.character <- function(x,
                               start = NULL,
                               end   = NULL,
                               by    = NULL,
                               times = NULL,
                               transducer_depth = NULL,
                               well_elevation   = NULL,
                               ...) {

  .check_files(x)

  start <- .check_times(start)
  end   <- .check_times(end)


  # read data
  sql_suffix <- generate_sql_times(start, end, by, times)
  db <- DBI::dbConnect(RSQLite::SQLite(), x)


  # get database info
  info <- rbr_info(db, x)


  # ensure same values used for all subsets if times or start, end, by are
  # provided
  if(sql_suffix != '') {
    sql_text <- paste0(generate_sql(db, start=NULL, end=NULL, by=NULL),
                       sql_suffix)
    dat <- read_rbr_db(db, x, sql_text)
  } else {
    sql_text <- generate_sql(db, start, end, by)
    dat <- read_rbr_db(db, x, sql_text)

    # raw_bin <- RSQLite::dbGetQuery(db, 'SELECT data FROM downloads')[[1]]
    #
    # dat <- data.table::data.table(file = x,
    #                               channel = info$channel[1],
    #                               data = list(read_rbr_bin(raw_bin, info$calibration[[1]]$value)))

  }


  RSQLite::dbDisconnect(db)


  if (!is.null(dat)) {

    # get file name without path
    # dat[, file_name := basename(file)]

    if (!is.null(by)) {
      dat[, by := by]
    }

    dat <- dat[info, on = c('file', 'channel')]

    return(dat)
  } else {
    return(NULL)
  }



  if(nrow(dat) > 0) {
    dat[, n := vapply(data, nrow, FUN.VALUE = integer(1))]
  } else {
    warning(paste0(x, " does not have any values in the selected range"))
  }


  if(!is.null(well_elevation)) {
    dat[, well_elevation := well_elevation]
  }

  if(!is.null(transducer_depth)) {
    dat[, transducer_depth := transducer_depth]
  }

  return(dat)
}





#' read_rbr
#'
#' @param locations file with port information
#' @param start starting date to get
#' @param end ending date to get
#' @param by spacing
#' @param use_rbr_tz boolean use time zone information from the rbr file?
#'
#' @return data.table of results
#' @export
#'
#' @import data.table
#'
#' @examples
read_rbr.data.table <- function(locations,
                                start = NULL,
                                end   = NULL,
                                by    = NULL,
                                times = NULL,
                                ...) {

  locs <- copy(locations)
  locs[, file_id := 1:nrow(locs)]

  dat <- locs[, read_rbr(file, start, end, by, times),
            by = file_id]

  nms <- c('file_id', setdiff(names(locs), names(dat)))
  dat <- dat[locs[, nms, with = FALSE], on = 'file_id']

  dat <- dat[n > 0]

  dat <- add_water_level(dat, ...)
  setcolorder(dat, c("file", "file_name", "model", "serial", "port", "is_baro",  "elevation",
                   "channel", "parameter", "version", "dt", "n", "units", "id", "data", "calibration"))

  dat
}

# system.time(a <-read_rbr('/home/jonathankennel/Storage/data/rbr/rd130 077623_20191119_1500.rsk'))
# fn <- list.files('/home/jonathankennel/Downloads/', pattern = 'rsk', full.names = TRUE)
#
# b <- read_rbr(fn[6])
# b
