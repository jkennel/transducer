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
#' @examples
#' db_name <- system.file("extdata", "example.rsk", package="transducer")
#' info <- rbr_info(db_name)
#===============================================================================
rbr_info <- function(db, db_name) {

  sql_text <- 'SELECT ruskinVersion AS ruskin FROM appSettings'
  version  <- setDT(RSQLite::dbGetQuery(db, sql_text))

  if (version$ruskin > '2.0.0') {
    sql_text <- 'SELECT calibrationID AS id, key, value FROM coefficients'
    coefficients <- setDT(RSQLite::dbGetQuery(db, sql_text))
    coefficients <- coefficients[, file := db_name]
    coefficients <- coefficients[, list(calibration = list(data.table(key, value))), by = list(file, id)]

    sql_text <- 'SELECT samplingPeriod AS dt FROM continuous'
    dt <- setDT(RSQLite::dbGetQuery(db, sql_text))
    dt <- dt[, file := db_name]

  } else {
    sql_text <- 'SELECT * FROM calibrations'

    coefficients <- setDT(RSQLite::dbReadTable(db, 'calibrations'))
    coefficients <- coefficients[, list(calibrationID, c0, c1, c2, c3, c4, c5,
                                        c6, c7, c8, x0, x1, x2, x3, x4, x5, x6,
                                        x7, n0, n1, n2, n3)]

    coefficients <- coefficients[, lapply(.SD, as.numeric), by = calibrationID]
    coefficients <- na.omit(
      melt(coefficients,
         id.vars = 'calibrationID',
         variable.name = 'key',
         value.name = 'value')
    )
    setnames(coefficients, 'calibrationID', 'id')
    coefficients[, file := db_name]
    coefficients <- coefficients[, list(calibration = list(data.table(key, value))), by = list(file, id)]

    sql_text <- 'SELECT samplingPeriod AS dt FROM schedules'
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


  coefficients[channel][serial, on = 'file'][dt, on = 'file']

}

