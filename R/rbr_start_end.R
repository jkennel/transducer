#' rbr_start_end
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rbr_start_end <- function (x, ...) {
  UseMethod("rbr_start_end", x)
}

#' rbr_start_end
#' start and end times for rsk data table
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
rbr_start_end.character <- function(x) {

  x <- x[file.exists(x)]

  start_id <- paste0("SELECT tstamp FROM data ORDER BY tstamp LIMIT 1")
  end_id <- paste0("SELECT tstamp FROM data ORDER BY tstamp DESC LIMIT 1")

  rbindlist(
    lapply(x, function(y) {
      db <- DBI::dbConnect(RSQLite::SQLite(), y)

      dt <- data.table(
        file_name = y,
        start = as.POSIXct(as.numeric(RSQLite::dbGetQuery(db, start_id)$tstamp)/1000,
                                    origin = '1970-01-01', tz = 'UTC'),
        end   = as.POSIXct(as.numeric(RSQLite::dbGetQuery(db, end_id)$tstamp)/1000,
                                    origin = '1970-01-01', tz = 'UTC'))

      RSQLite::dbDisconnect(db)
      return(dt)
    })
  )
}



#' rbr_start_end
#' start and end times for rsk data table
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
rbr_start_end.data.table <- function(x) {

  rbr_start_end(x$file)

}
