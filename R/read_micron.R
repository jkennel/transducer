#' read_micron
#'
#' @param x the full path to the file
#' @param calibration data table with calibration points
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_micron <- function(x,
                        transducer_depth = NULL,
                        well_elevation   = NULL,
                        calibration = NULL, ...) {
  .check_files(x)

  n_sheets <- length(readxl::excel_sheets(x))

  data <- as.data.table(readxl::read_excel(path = x,
                                          sheet = n_sheets,
                                          skip = 7,
                                          col_names = c('datetime', 'value'),
                                          col_types = c('date', 'skip', 'numeric')
  ))

  data[value < 0, value := NA_real_]

  h <- unlist(readxl::read_excel(x,
                                 sheet = n_sheets,
                                 range = "C1:C4",
                                 col_types = 'text',
                                 col_names = 'info'))

  if(is.null(calibration)) {
    calibration <- data.table(coef = character(), value = numeric())
    units <- 'milliampere'
  } else {

    calibration <- copy(calibration)[serial == h[3]]

    intercept <- calibration[['intercept']]
    slope <- calibration[['slope']]

    data[, value := intercept + slope * value]

    units <- calibration[['units']][1]
  }


  dat <- data.table::data.table(
    file = x,
    channel = NA_character_,
    data = list(data),
    id = h[4],
    calibration = list(calibration),
    parameter = 'pressure',
    units = units,
    version = h[2],
    serial = h[3],
    model = h[1],
    dt = unique(diff(as.numeric(data[['datetime']][1:2])))
  )

  if(!is.null(well_elevation)) {
    dat[, well_elevation := well_elevation]
  }

  if(!is.null(transducer_depth)) {
    dat[, transducer_depth := transducer_depth]
  }

  dat

}



#' micron_calibration
#'
#' @param x file path
#' @param conversion_constant conversion constant default is psi to m H2O
#' @param units after conversion
#' @param robust use MASS::rlm method instead of lm
#'
#' @return
#' @export
#'
#' @examples
micron_calibration <- function(x,
                               conversion_constant = 0.703070,
                               units = 'meters',
                               robust = TRUE) {

  calib <- data.table::fread(x)

  calib[, datetime := as.POSIXct(datetime, tz = 'UTC')]
  calib[, meters := psi * conversion_constant]
  if(robust) {
    calib_fit <- calib[, .(fit = list(MASS::rlm(meters~milliampere, .SD))), by = list(datetime, serial)]
  } else {
    calib_fit <- calib[, .(fit = list(stats::lm(meters~milliampere, .SD))), by = list(datetime, serial)]
  }


  calib_fit[, sigma := lapply(fit, function(x) summary(x)[['sigma']])]
  calib_fit[, intercept := unlist(lapply(fit, function(x) coefficients(x)[1]))]
  calib_fit[, slope := unlist(lapply(fit, function(x) coefficients(x)[2]))]
  calib_fit[, data := split(calib, calib$serial)]
  calib_fit[, units := units]

  calib_fit

}

