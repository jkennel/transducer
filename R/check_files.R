#' @title  .check_files
#'
#' @details This function checks the input to be read to see if files exist
#' and the format is correct.
#'
#' @param x file path(s) of the transducers
#'
#' @return error for invalid inputs
#'
#' @keywords internal
#' @export
#'
#' @examples
#' .check_files(NULL)
#' .check_files(as.character(rnorm(1)))
#' .check_files(rnorm(1))
.check_files <- function(x) {


  # check zero length
  if (length(x) == 0) {
    stop('input x is of zero length.')
  }

  # check class
  if (!is.character(x)){
    stop('Input x must be a character path to file(s).')
  }


  for(i in seq_along(x)) {

    # check if files exist
    if (!file.exists(x[i])) {
      stop(paste0(x[i], ' file does not exist.'))
    }

    # check empty file
    if (file.size(x[i]) == 0) {
      stop(paste0(x[i], ' is an empty file'))
    }

    # check empty data base
    if(tools::file_ext(x[i]) == 'rsk') {

      db <- RSQLite::dbConnect(RSQLite::SQLite(), x[i])
      tbls <- RSQLite::dbListTables(conn = db)
      RSQLite::dbDisconnect(db)

      if (!'data' %in% tbls) {
        stop(paste0(x[i], ' is does not have a data table'))
      }

    }

  }


  invisible(x)
}
