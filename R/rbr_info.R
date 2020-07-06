#===============================================================================
#' @title rbr_info
#'
#' @description get supplementary info from .rsk file
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param x path to .rsk file
#' @param ...
#' @param db database connection
#' @param db_name character the path to the rbr database ( rsk )
#'
#' @return list of results
#'
#' @export
#'
#===============================================================================
rbr_info <- function (x, ...) {
  UseMethod("rbr_info", x)
}


#===============================================================================
#' @title rbr_info
#'
#' @description get supplementary info from .rsk file
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param x path(s) to .rsk file
#'
#' @return list of results
#'
#' @export
#'
#===============================================================================
rbr_info.character <- function(x) {


  info_dt <- lapply(x, function(db_name) {

    db <- DBI::dbConnect(RSQLite::SQLite(), db_name)

    info <- rbr_info(db, db_name)

    RSQLite::dbDisconnect(db)

    return(info)
  })

  rbindlist(info_dt)


}


#===============================================================================
#' @title rbr_info
#'
#' @description get supplementary info from .rsk file
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db database connection
#' @param db_name character the path to the rbr database ( rsk )
#'
#' @return list of results
#'
#' @export
#'
#===============================================================================
rbr_info.SQLiteConnection <- function(db, db_name) {

  # Check if coefficient table exists
  nm_tbl <- RSQLite::dbListTables(db)

  is_empty <- TRUE

  if ('appSettings' %in% nm_tbl){
    sql_text <- 'SELECT ruskinVersion AS ruskin FROM appSettings'
    version  <- data.table::setDT(RSQLite::dbGetQuery(db, sql_text))
    if(nrow(version) > 0) {
      is_empty <- FALSE
    }
  }

  if(!'appSettings' %in% nm_tbl | is_empty) {
    return(data.table(file = db_name,
                      id = NA_integer_,
                      calibration = list(data.table(key1 = NA_character_,
                                                    value = NA_real_)),
                      channel = NA_character_,
                      type = NA_character_,
                      units = NA_character_,
                      ruskin_version = NA_character_,
                      serial = NA_integer_,
                      model = NA_character_,
                      dt = NA_integer_))

  }



  if ('coefficients' %in% nm_tbl) {
    sql_text <- 'SELECT * FROM coefficients'
    coefficients <- data.table::setDT(RSQLite::dbGetQuery(db, sql_text))
    if('key' %in% names(coefficients)) {
      use_coefficient_table <- TRUE
    } else {
      use_coefficient_table <- FALSE
    }
  } else {
    use_coefficient_table <- FALSE
  }



  # this check needs to be done more rigorously!!
  if (use_coefficient_table) {
    coefficients <- coefficients[, list(id = calibrationID, key, value = as.numeric(value))]
    coefficients <- coefficients[, file := db_name]
    coefficients <- coefficients[, list(calibration = list(data.table(key, value))), by = list(file, id)]

    if('continuous' %in% nm_tbl) {
      sql_text <- 'SELECT continuousID AS id, samplingPeriod AS dt FROM continuous'
      dt <- setDT(RSQLite::dbGetQuery(db, sql_text))
      dt <- dt[, file := db_name]
    } else if ('schedules' %in% nm_tbl){
      sql_text <- 'SELECT scheduleID AS id, samplingPeriod AS dt FROM schedules'
      dt <- setDT(RSQLite::dbGetQuery(db, sql_text))
      dt <- dt[, file := db_name]
    }

  } else {

    sql_text <- 'SELECT * FROM calibrations'
    coefficients <- setDT(RSQLite::dbReadTable(db, 'calibrations'))

    # some calibration files don't have all the constants
    # set missing values to NA
    setnames(coefficients, names(coefficients), gsub('coef0','c', names(coefficients)))
    coef_nms <- c('c0', 'c1', 'c2', 'c3', 'c4', 'c5',
                  'c6', 'c7', 'c8', 'x0', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6',
                  'x7', 'n0', 'n1', 'n2', 'n3')
    coef_nms <- setdiff(coef_nms, names(coefficients))

    if(length(coef_nms) > 0) {
      coefficients[, (coef_nms) := NA_real_]
    }

    coefficients <- coefficients[, list(calibrationID, c0, c1, c2, c3, c4, c5,
                                        c6, c7, c8, x0, x1, x2, x3, x4, x5, x6,
                                        x7, n0, n1, n2, n3)]

    coefficients <- coefficients[, lapply(.SD, as.numeric), by = calibrationID]
    coefficients <- na.omit(
      melt(coefficients,
         id.vars = 'calibrationID',
         variable.name = 'key',
         value.name = 'value',
         variable.factor = FALSE)
    )
    setnames(coefficients, 'calibrationID', 'id')
    coefficients[, file := db_name]
    coefficients <- coefficients[, list(calibration = list(data.table(key, value))), by = list(file, id)]

    sql_text <- 'SELECT scheduleID AS id, samplingPeriod AS dt FROM schedules'
    dt <- setDT(RSQLite::dbGetQuery(db, sql_text))
    dt <- dt[, file := db_name]
  }

  sql_text <- 'SELECT serialID AS serial, model FROM instruments'
  serial <- setDT(RSQLite::dbGetQuery(db, sql_text))
  serial[, file := db_name]

  sql_text <- 'SELECT channelID AS id, shortName AS channel, longName AS type, units FROM channels'
  channel <- setDT(RSQLite::dbGetQuery(db, sql_text))
  channel[, file := db_name]
  channel[, ruskin_version := version]
  channel[, channel := tolower(channel)]
  channel[, type := tolower(type)]


  setkey(serial, file)
  setkey(channel, file, id)
  setkey(coefficients, file, id)
  setkey(dt, file, id)

  dt <- unique(dt[, list(file, dt)])
  coefficients[channel][serial, on = 'file'][dt, on = 'file']

}

