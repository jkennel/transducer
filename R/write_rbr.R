#' write_rbr
#'
#' @param x output from read_rbr
#'
#' @return
#' @export
#'
#' @examples
write_rbr <- function(x, add_text){

  for (i in 1:nrow(x)) {
    fn <- tail(strsplit(x$file[i], '/')[[1]], 1)
    fn <- gsub('.rsk', '.csv', fn)
    fn <- paste0(x$channel[i], '_', fn)
    data.table::fwrite(x$data[[i]], file = fn)
  }

}

