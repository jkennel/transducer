#===============================================================================
#' .parse_westbay_header
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#===============================================================================
.parse_westbay_header <- function(x) {


  download_time <- .get_westbay_download_time(h)
  probe_info    <- .get_westbay_probe_info(h)
  port_info     <- .get_westbay_port_info(h)
  elevation     <- .get_westbay_elevation(h)


  h <- data.table(download_time,
                  probe_info = list(probe_info),
                  port_info  = list(port_info),
                  elev       = elevation$elev,
                  elev_unit  = elevation$units)


  invisible(h)
}



#===============================================================================
.get_westbay_download_time <- function(x) {

  wh <- which(grepl('Date of Export:', x, fixed = TRUE))

  tz <- regmatches(x[wh], gregexpr("(?<=\\().*?(?=\\))", x[wh], perl=T))[[1]]
  tz <- gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', tz ,perl = TRUE)

  download_time <- as.POSIXct(gsub("[^0-9.]", "",  x[wh]), format = '%Y%m%d%H%M%OS', tz = tz)

}



#===============================================================================
.get_westbay_elevation <- function(x) {

  wh <- which(grepl('Elevation:', x, fixed = TRUE))
  units <- regmatches(x[wh], gregexpr("(?<=\\().*?(?=\\))", x[wh], perl=TRUE))[[1]]

  elev <- gsub("[^0-9.]", "",  x[wh])

  data.table(elev = as.numeric(elev), units)

}
