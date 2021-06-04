#' @title  .check_times
#'
#' @details This function checks the class of the datetime
#'
#' @param x either a character string or datetime object
#'
#' @return error for invalid inputs
#'
#' @keywords internal
#' @export
#'
#' @examples
#' .check_times('2018-01-01')
.check_times <- function(x) {

  # convert character input times
  if(!is.null(x)) {
    if (inherits(x, 'character')) {
      x <- as.POSIXct(x, tz = 'UTC')
    }
  }

  x

}
