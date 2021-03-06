#===============================================================================
#' @title obtain data from rbr sqlite3 database
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @inheritParams read_rbr
#' @param db database connection
#'
#' @param sql_text character sql string to execute on connection for filtering
#'
#' @return data.table of results
#'
#' @export
#'
#===============================================================================
read_rbr_db <- function(db, db_name, sql_text) {
  # .rsk stores date as numeric and is referenced to UTC
  # hack for 'global variables NOTE
  datetime <- NULL

  # connect to sqlite database
  nm_tbl <- RSQLite::dbListTables(db)


  if(!'data' %in% nm_tbl) {

    return(data.table(file = db_name,
               channel = NA,
               data = list(data.table(value = NA_real_,
                                      datetime = as.POSIXct(NA_real_, origin = '1970-01-01')))))
  }

  # get column names
  if (!any(grepl('channels', nm_tbl))) {
    warning(paste(db_name, 'does not have a table called "channels".  Check .rsk file.'))
    return(data.table(file = db_name,
                      channel = NA,
                      data = list(data.table(value = NA_real_,
                                             datetime = as.POSIXct(NA_real_, origin = '1970-01-01')))))
  } else {
    channels <- unique(RSQLite::dbGetQuery(db, 'SELECT shortName FROM channels')[[1]])
  }

  # check if any data is present
  if (!any(grepl('data', nm_tbl))) {
    warning(paste(db_name, 'does not have a table called "data".  Check .rsk file.'))
    return(NULL)
  }


  # time is in milliseconds
  # read data into data.table and set key
  dt <- tryCatch(data.table::setDT(RSQLite::dbGetQuery(db, sql_text), key = 'tstamp'), error = function(e) e)

  if (inherits(dt, "error")) {
    warning(paste(db_name, ' data table is malformed.'))
    return(data.table(file = db_name,
                      channel = tolower(channels),
                      data = list(data.table(value = NA_real_,
                                             datetime = as.POSIXct(NA_real_, origin = '1970-01-01')))))
  }
  if (nrow(dt) <= 0) {
    warning(paste(db_name, 'returns no rows for the given query.'))
    return(data.table(file = db_name,
                      channel = tolower(channels),
                      data = list(data.table(value = NA_real_,
                                             datetime = as.POSIXct(NA_real_, origin = '1970-01-01')))))
  }






  data.table::setnames(dt, 'tstamp', 'datetime')


  dt[, datetime := as.POSIXct(datetime/1000,
                              origin = '1970-01-01',
                              tz = 'UTC')]

  data.table::setnames(dt, c('datetime', channels))
  setkey(dt, datetime)

  if(ncol(dt) > 2) {
    dt <- melt(dt, id.vars = 'datetime')

    return(
      data.table(file = db_name,
                 channel = tolower(channels),
                 data = split(dt, by = 'variable', keep.by = FALSE))
    )
  } else {
    setnames(dt, c('datetime', 'value'))
    return(
      data.table(file = db_name,
                 channel = tolower(channels),
                 data = list(dt))
    )
  }

}


