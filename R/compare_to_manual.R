#' compare_manual
#' This is for when all non-barometric transducers should read the same depth adjusted value
#'
#' @param location
#' @param manual_wl
#' @param stat_fun
#'
#' @return
#' @export
#'
#' @examples
compare_manual <- function(location,
                           manual_wl,
                           stat_fun = median) {

  man_wl <- location[manual_wl, on = 'well', nomatch = 0]

  comp <- read_rbr(man_wl)
  comp <- comp[, estimate_wl(.SD, start, end, stat_fun),
                    by = list(start, end, well, value_manual)]

  comp[, shift := 0.0]
  comp <- comp[location, is_baro := i.is_baro, on = 'file']
  comp[is_baro == FALSE, shift := value_manual - value_est, on = 'file']

  comp[!is.na(value_est)]


}


#' compare_air
#' This is for when all transducers should read the same value
#'
#' @param location
#' @param manual_wl
#' @param stat_fun
#'
#' @return
#' @export
#'
#' @examples
compare_air <- function(location, manual_wl, stat_fun = median) {

  man_wl <- location[manual_wl, on = 'well']

  man_wl[, estimate_air(.SD, start, end, stat_fun),
            by = list(start, end, well)]

}


#' estimate_air
#'
#' @param locations
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
estimate_air <- function(locations, start, end, stat_fun = median) {

  r <- read_rbr(locations$file, start, end)
  r[, value_air := as.numeric(lapply(data, function(x) stat_fun(x$value, na.rm = TRUE)))]

  r[, shift :=  stat_fun(value_air, na.rm = TRUE) - value_air]

  return(r[, list(file, value_air, shift)])

}


#' estimate_wl
#'
#' @param locations
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
estimate_wl <- function(x, start, end, stat_fun = median) {

  #r <- read_rbr(locations, start, end)

  x <- copy(x)[, value_est := NA_real_]

  x[,
    value_est := as.numeric(lapply(data, function(y) {
      stat_fun(y$value_wl, na.rm = TRUE)
      }))
  ]

  #x[is_baro == TRUE & type == 'pressure', value_est := 0.0]


  return(x[, list(file, value_est)])

}

#' adjust_value
#'
#' @param x
#' @param shift
#'
#' @return
#' @export
#'
#' @examples
adjust_value <- function(x, shift) {

  x[shift, shift := i.shift, on = c('file', 'well')]

  x[, `:=` (data = lapply(data, adj_wl, adjust = shift)),
    by = seq_len(nrow(x))]

}



adj_wl <- function(z, adjust) {

  z <- copy(z)

  z[, value_adj := (value_wl + adjust)]

  return(z)
}
