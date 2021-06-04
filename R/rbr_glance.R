#===============================================================================
#' rbr_glance
#'
#' Create a quick plot of thumbnail data
#'
#' @param x
#'
#' @return thumbnail data.table
#' @export
#'
#' @examples
#===============================================================================
rbr_glance <- function(x) {

  # get thumbnail table
  db <- RSQLite::dbConnect(RSQLite::SQLite(), x)

  th <- RSQLite::dbGetQuery(db, "SELECT * FROM downsample100")
  th <- data.table::as.data.table(th)

  RSQLite::dbDisconnect(db)


  th[, datetime := as.POSIXct(tstamp/1000, origin = '1970-01-01', tz = 'UTC')]
  th[, tstamp := NULL]


  th <- data.table::melt(th, id.vars = 'datetime')

  print(plotly::plot_ly(th,
                        x = ~datetime,
                        y = ~value,
                        color = ~variable,
                        type = 'scatter',
                        mode = 'lines'))

  invisible(th)

}



