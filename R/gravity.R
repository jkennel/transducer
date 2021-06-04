#===============================================================================
#' gravity
#'
#' @param elevation the elevation of the monitoring location in meters
#' @param latitude the latitude for the monitoring location in degrees
#'
#' @return
#' @export
#'
#' @seealso \link{https://www.sensorsone.com/local-gravity-calculator/}
#'
#' @examples
#' gravity()
#' gravity(1000, 45)
#===============================================================================
gravity <- function(elevation = 0, latitude = 0) {

  latitude <- latitude * pi/180

  g1 <- 9.780327 * (1 + 0.0053024 * sin(latitude)^2 - 0.0000058 * sin(2*latitude)^2)
  g2 <- -3.086e-6 * elevation

  g1 + g2

}




# y <- seq(-100:100)
# x <- -y + y^2 + -y^3
# x <- 1:20
# density_temperature(x)
#
#
# g <- 9.8
# rho <- 999.9128
# g * rho
# rho <- 997.9355
# g * rho
#
# N/m2
# N/m3
#
#
# g <- 9.8
# rho <- 999.9128
# (10000 - 0) / (g * rho)
# g <- 9.8
# rho <- 997.9355
# (10000 - 0) / (g * rho)
#
#
#
#
# m/s2 * kg/m3
# kg/s2m2
#
#
# plot(temperature(seq(0, 20, 0.1)), type='l')
#
#
#
#
#
# gal_to_si <- function(x) {
#   3.281e-2 * x * 0.3048
# }
#
#
#
#
#
#
#
# gravity(100, 0) * 1000
# gravity(300, 0) * 1000


