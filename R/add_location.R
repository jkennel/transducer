# -------------------------------------------------------------------------
# Location table must have
#   file_name: the name of the file without the path
#   elevation: the elevation of the transducer in (m)
# -------------------------------------------------------------------------
#' add_location
#'
#' @param x data.table
#' @param locations data.table with file_name, in_water, and elevation
#'
#' @return
#' @export
#'
#' @examples
add_location <- function(x, locations, on_string = 'serial') {

  locations[x, on = on_string]

}

#' to_pore_pressure
#'
#' @param x data.table
#' @param dbar_to_m value to use for dbar to m H2O conversion
#'
#' @return
#' @export
#'
#' @examples
add_pore_pressure <- function(x, dbar_to_m = NULL) {

  x[, `:=` (data = lapply(data, to_pore_pressure,
                          elevation = elevation,
                          units = units,
                          dbar_to_m = dbar_to_m)),
    by = seq_len(nrow(x))]

  x

}


#' add_water_level
#'
#' If baro_serial is not provided this function will try to use a baro logger in
#' the provided dataset.
#'
#'
#' @param x
#' @param baro_serial
#' @param dbar_to_m
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_water_level <- function(x, baro_serial = NULL, dbar_to_m = 1.019716, ...) {


  # check if baro serial number is provided
  if(is.null(baro_serial)) {

    # check if there are any baro file in dataset
    if(any(x$is_baro)) {
      baro <- x[is_baro == TRUE & type == 'pressure']
    } else {
      baro <- NA
    }

  } else {
    baro <- x[serial == baro_serial & type == 'pressure']
  }

  n <- sum(x$type == 'pressure')

  if(n == 0) {
    warning('No pressure measurements in dataset.')
    return(x)
  }

  x[, `:=` (data = lapply(data, to_water_level,
                          well = well,
                          type = type,
                          baro = baro,
                          elevation = elevation,
                          units = units,
                          dbar_to_m = dbar_to_m,
                          is_baro = is_baro)),
    by = seq_len(nrow(x))]

  x

}


to_pore_pressure <- function(z, elevation, units, dbar_to_m = 1.019716) {

  z <- copy(z)

  if (units == 'dbar') {
    z[, value_pore_m := (value * dbar_to_m) + elevation]
  }

  return(z)
}

to_water_level <- function(z,
                           well,
                           type,
                           baro,
                           elevation, units,
                           dbar_to_m = 1.019716,
                           is_baro = FALSE) {



  if(type != 'pressure') {
    z <- copy(z)[, value_wl := NA_real_]

    return(z)
  }

  if (is.na(is_baro)) {
    z <- copy(z)[, value_wl := value * dbar_to_m]
    return(z)
  }

  if (is_baro != FALSE) {
    z <- copy(z)[, value_wl := value * dbar_to_m]
    return(z)
  }

  if(all(is.na(baro))) {
    warning('No barometric pressure value was provided. Converting dbar to meters only')
    if (units == 'dbar') {
      z <- copy(z)[, value_wl := (value * dbar_to_m) + elevation]
      return(z)
    }
  } else if (class(baro)[1] == "data.table") {
    if(well %in% baro$well) {
      baro <- rbindlist(baro[well == well]$data)
    } else {
      warning(paste0("Well name ",  well, " not found in barometric file. Using aggregated barometric record"))
      baro <- rbindlist(baro$data)
      baro <- baro[, list(value = mean(value, na.rm = TRUE)), by = list(datetime)]
    }
  }

  if (length(intersect(baro$datetime, z$datetime)) == 0) {
    warning('No barometric pressure datetimes match water level datetimes. Converting dbar to meters only')
    z <- copy(z)[, value_wl := (value * dbar_to_m) + elevation]
    return(z)
  }

  z <- copy(z)[baro, value_wl := value - i.value, on = 'datetime']

  if (units == 'dbar') {
    z[, value_wl := (value_wl * dbar_to_m) + elevation]
  }

  return(z)

}


#
#   to_meter <- function(x, units, ) {
#     if (units == 'dbar') {
#       x$value_m = measurements::conv_unit(x$value, 'dbar', 'cmH2O') / 100
#     }
#
#     return(x)
#   }
#
#
#   to_pore_pressure <- function(x, y) {
#
#     x$value_pore = x$value_m + y
#
#     return(x)
#   }
#
#   to_water_level <- function(x, y) {
#
#     x$value_wl = x$value_pore - x$value_baro
#
#     return(x)
#   }
#
# }


# library(data.table)
# library(dplyr)
# library(transducer)
# db_name <- list.files( '/media/kennel/Data/phd/personnel/pat/syngenta/', full.names = TRUE, pattern = '*.rsk')
# system.time(rbr_start_end(db_name))
#
# locations <- fread('/media/kennel/Data/phd/personnel/pat/syngenta/transducer_depth_path.csv')
# locations[, file_name := tail(strsplit(path, '/')[[1]], 1), by = 1:nrow(locations)]
# locations <- locations[, list(well, port = id, elevation = 300-depth, in_water = baro, file_name, serial)]

# system.time(
#   x <- read_rbr(db_name, by = 3600*3)
# )


# system.time(
#   x <- read_rbr(db_name,
#                 start = as.POSIXct('2014-12-15', tz = 'UTC'),
#                 end   = as.POSIXct('2015-04-01', tz = 'UTC'), by = 3600)
# )

#
#
# system.time({
#
#   x <- read_rbr(db_name,
#                 start = as.POSIXct('2014-12-15', tz = 'UTC'),
#                 end   = as.POSIXct('2014-12-16', tz = 'UTC'),
#                 by = 600) %>%
#     add_location(locations, on_string = c('file_name', 'serial')) %>%
#     add_water_level(baro_serial = 77653)
#
# })
#
#
# setDTthreads(1)
# system.time(tmp1 <-read.rsk('/media/kennel/Data/phd/personnel/pat/syngenta/SYN UW1 Baro P1 - 077653_20150410_1435.rsk',
#                             from = start, to = end + 86400*5))
# system.time(tmp2 <-read_rbr('/media/kennel/Data/phd/personnel/pat/syngenta/SYN UW1 Baro P1 - 077653_20150410_1435.rsk',
#                             start = start, end = end + 86400*5))

#
# library(oce)
# system.time(tmp1 <-read.rsk('/media/kennel/Data/phd/personnel/pat/syngenta/SYN UW1 Baro P1 - 077653_20150410_1435.rsk'))
# system.time(tmp2 <-read_rbr('/media/kennel/Data/phd/personnel/pat/syngenta/SYN UW1 Baro P1 - 077653_20150410_1435.rsk'))



# # add_location(x, locations, on_string = 'serial')
#
# x <- db_name %>%
#   read_rbr(max_n = 10000) %>%
#   add_location(locations, on_string = 'serial') %>%
#   to_pore_pressure()
#
#
#   to_water_level(a)

