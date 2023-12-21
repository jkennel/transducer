#' read_transducer
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_transducer <- function(x, ...) {


  # check for valid input
  .check_files(x)


  dat <- rbindlist(lapply(x, switch_reader, ...))

  dat

}


#' switch_reader
#'
#' @param z
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
switch_reader <- function(z, ...) {

  # get file extension
  f_ext   <- tolower(tools::file_ext(z))
  ext     <- c('rsk', 'xle', 'lev', 'dat', 'mon', 'csv', 'xlsx')
  ext_ind <- which(ext %in% f_ext)


  # choose the transducer type
  switch(EXPR = which(ext %in% f_ext),
         read_rbr(z, ...),
         read_levelogger(z, ...),
         read_diver(z, ...),
         read_diver(z, ...),
         read_diver(z, ...),
         read_westbay(z, ...),
         read_micron(z, ...))

}

