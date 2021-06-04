#' Remove the calibration parameters from the rbr to get raw readings
#'
#' @param rbr
#'
#' @return
#' @export
#'
#' @examples
rbr_estimate_millivolt <- function(rbr) {

  cal    <- rbindlist(rbr$calibration)
  dat_mv <- rbindlist(rbr$data)

  # Equation c0 + c1*x + c2*x^2 + c3*x3 = y for pressure

  undo <- function(p, y, co) {
    mv = p[1]

    est <- co[1] + co[2]*mv + co[3]*mv^2 + co[4]*mv^3
    sum((y-est)^2)

  }

  co <- cal$value
  p <- 1
  r <- range(dat_mv[['value']], na.rm = TRUE)
  y <- seq(r[1], r[2], length.out = 20000)
  x <- c()

  for (i in seq_along(y)) {
    x[i] <- nlm(undo, p, y = y[i],
                co = co,
                gradtol = 1e-12,
                ndigit = 15,
                steptol = 1e-12)$estimate
  }



  dat_mv[, mv := approx(x = y, y = x, xout = value)$y]

  rbr[, data := .(dat_mv)]

  rbr

}



# # library(transducer)
# # library(data.table)
# #
# library(dplyr)
# library(DBI)
# con <- dbConnect(RSQLite::SQLite(), '/home/jonathankennel/Storage/data/rbr/rd130 077623_20191119_1500.rsk')
# tbl_names <- db_list_tables(con)
# dbListObjects(con)
# dbListFields(con, 'power')
# tbl(con, 'power')
# tt <- tbl(con, 'data')
#
# tbl_name <- 'coefficients'
# rs <- dbSendQuery(con, paste0("SELECT * FROM ", tbl_name))
# cal <- as.numeric(dbFetch(rs)$value)
#
#
# for (i in seq_along(tbl_names)){
#   tbl_name <- tbl_names[i]
#   tbl_name <- 'data'
#   rs <- dbSendQuery(con, paste0("SELECT * FROM ", tbl_name))
#   system.time(
#   system.time(xd <- dbFetch(rs))
#   )
#   tbl_name <- 'downloads'
#   rs <- dbSendQuery(con, paste0("SELECT * FROM ", tbl_name))
#   dl <- dbFetch(rs)
#
#   system.time({
#     aa <- list()
#   for(i in 1:nrow(dl)) {
#     tmp <- readBin(dl$data[[i]],
#                    n = 1000000L,
#                    what = 'integer',
#                    size = 4L,
#                    endian = 'little')
#     x <- (tmp/2^30)
#     aa[[i]] <- data.table(val = c[1] + c[2]*x + c[3]*x^2 + c[4]*x^3)
#     #aa[[i]] <- data.table(val = x)
#     # print(head(aa))
#   }
#     aa <- rbindlist(aa)
#     aa[, datetime := as.POSIXct('1970-01-01', tz = 'UTC')+1:nrow(aa)]
#     # aa[[i]] <- data.table(val = c[1] + c[2]*x + c[3]*x^2 + c[4]*x^3)
#
#   })
#
#   print(tbl_name)
#   print(nrow(rs))
# }



# system.time(dat <- read_rbr_bin(dl$data, cal))
#
#
# rbr <- read_transducer('/home/jonathankennel/Storage/data/rbr/rd130 077623_20191119_1500.rsk')
# rbr <- rbr_estimate_millivolt(rbr)
#
#
# test <- fread('/home/jonathankennel/Storage/analyses/calibration/test.csv')
#
# lin_fit_rbr <- lm(y~I(x), test)
# quad_fit_rbr <- lm(y~I(x) + I(x^2), test)
# cube_fit_rbr <- lm(y~I(x) + I(x^2) + I(x^3), test)
#
# test[, res_lin := residuals(lin_fit_rbr)]
# test[, res_quad := residuals(quad_fit_rbr)]
# test[, res_cube := residuals(cube_fit_rbr)]
# plot(res_lin~x, test, type='o',
#      ylab = 'Residuals (dbar)',
#      xlab = 'Millivolt')
# points(res_quad~x, test, type='o', col = 'red')
# points(res_cube~x, test, type='o', col = 'blue')
#
#
#
#
# test[, noise := rnorm(length(y), mean = 0, sd = 0.002)]
#
# lin_fit <- MASS::rlm(y+noise~I(x), test)
# quad_fit <- MASS::rlm(y+noise~I(x) + I(x^2), test)
# cube_fit <- MASS::rlm(y+noise~I(x) + I(x^2) + I(x^3), test)
#
# test[, pred_lin := predict(lin_fit)]
# test[, pred_quad := predict(quad_fit)]
# test[, pred_cube := predict(cube_fit)]
#
# test[, res_lin := y-pred_lin]
# test[, res_quad := y-pred_quad]
# test[, res_cube := y-pred_cube]
#
#
# plot(res_lin~x, test, type='o',
#      ylab = 'Residuals (dbar)',
#      xlab = 'Millivolt', ylim = c(-0.1, 0.2))
# points(res_quad~x, test, type='o', col = 'red')
# points(res_cube~x, test, type='o', col = 'blue')
#
# #
# # d <- rbr$data[[1]][(as.numeric(datetime) %% 1800) == 0]
# # d[, lin_pred := predict(lm(value~mv))]
# # d[, lin_res := residuals(lm(value~mv))]
# #
# # library(ggplot2)
# # tmp <- melt(d, id.vars = 'datetime')
# #
# # ggplot(tmp, aes(x = datetime, y = value)) +
# #   geom_line() +
# #   facet_wrap(variable~., ncol = 1, scales = 'free_y') +
# #   theme_bw()

