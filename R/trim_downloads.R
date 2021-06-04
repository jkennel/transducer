
#' trim_downloads
#'
#' @param x
#' @param n_vals
#'
#' @return
#' @export
#'
#' @examples
trim_downloads <- function(x, n_vals = c(5, 5)) {

  if(length(n_vals == 1)) {
    n_vals <- rep(n_vals, 2)
  } else if (length(n_vals) > 2) {
    stop('n_vals must have length 1 or 2')
  }

  (x)[, data := lapply(data, function(y) {
    if(nrow(y) != 0){
      y <- y[!between(datetime, min(datetime, na.rm = TRUE) + n_vals[1],
                max(datetime, na.rm = TRUE) - n_vals[2]),
                value := NA_real_]
    }
    y

  })]

  x

}
