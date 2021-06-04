#' write_rbr
#'
#' @param x output from read_rbr
#' @param folder_path the folder to export files to
#' @param datetime_format one of "ISO","squash","epoch","write.csv"
#' @param ... arguments to pass to fwrite
#'
#' @return
#' @export
#'
write_rbr <- function(x, folder_path, datetime_format = 'write.csv', ...) {

  for (i in 1:nrow(x)) {
    fn <- tail(strsplit(x$file[i], '/')[[1]], 1)
    fn <- gsub('.rsk', '.csv', fn)
    fn <- paste0(x$channel[i], '_', fn)
    data.table::fwrite(x$data[[i]], file = file.path(folder_path, fn), dateTimeAs = datetime_format, ...)
  }

  invisible(x)

}


#' write_rbr_info
#'
#' @param x the data.table of data
#' @param file_path the output file path
#' @param ...
#'
#' @return
#' @export
#'
write_rbr_info <- function(x, file_path, ...) {

  data.table::fwrite(x[, -c('data', 'calibration'), with = FALSE],
                     file = file_path, ...)

  invisible(x)

}


