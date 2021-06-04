#' #' @title  density_temperature
#' #'
#' #' @details This function estimates the density of water at a given temperature
#' #' using Kell's formulation. ITS-90
#' #'
#' #' @param x temperature
#' #'
#' #' @return
#' #' @export
#' #' @seealso \link{https://nvlpubs.nist.gov/nistpubs/jres/097/jresv97n3p335_A1b.pdf}
#' #'
#' #' @examples
#' density_temperature <- function(x) {
#'   (999.83952 + 16.945176 * x - 7.9870401e-3*x^2 -46.170461e-6*x^3 + 105.56302e-9*x^4 - 280.54253e-12*x^5) / (1.0 + 16.897850e-3*x)
#' }
