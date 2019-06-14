#' preview
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
read_rbr.character <- function(db_name,
                               start = NULL,
                               end   = NULL,
                               by    = NULL,
                               times = NULL) {

  db_name <- db_name[file.exists(db_name)]

  # read data
  sql_suffix <- generate_sql_times(start, end, by, times)

  rbindlist(
    lapply(
      db_name,
      function(x) {
        db <- DBI::dbConnect(RSQLite::SQLite(), x)

        # get database info
        info <- rbr_info(db, x)

        # ensure same values used for all subsets if times or start, end, by are
        # provided
        if(sql_suffix != '') {
          sql_text <- paste0(generate_sql(db, start=NULL, end=NULL, by=NULL),
                             sql_suffix)
        } else {
          sql_text <- generate_sql(db, start, end, by)
        }

        dt <- read_rbr_db(db, x, sql_text)

        RSQLite::dbDisconnect(db)

        # get file name without path
        dt[, file_name := tail(strsplit(file, '/')[[1]], 1), by = 1:nrow(dt)]

        if (!is.null(by)) {
          dt[, by := by]
        }

        dt <- dt[info, on = c('file', 'channel')]

        return(dt)
      }))

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
                                times = NULL) {


  dat <- read_rbr(locations$file,
                  start = start,
                  end   = end,
                  by    = by,
                  times = times)

  add_water_level(locations[dat, on = 'file'])

}
