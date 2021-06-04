# library(ssfldata)
# library(data.table)
# library(R6)
# library(transducer)
#
# data(rbr_files)
# ba_files <- rbr_files[name == 'RD-130' & grepl('baro', type)]
# wl_files <- rbr_files[name == 'RD-130' & !grepl('baro', type)]
#
# ba <- read_transducer(
#   file.path('/media/jonathankennel/Seagate Expansion Drive/rbr_g360',
#             ba_files$fn))
# wl <- read_transducer(
#   file.path('/media/jonathankennel/Seagate Expansion Drive/rbr_g360',
#             wl_files$fn))
#
# x <- rbindlist(wl[['data']])
# setkey(x, datetime)
#
# aa <- system.time(find_gaps(x))

# steps
# filter duplicates
# find gaps
# make regular
# add NA
# fill gaps

# filter_duplicates ------------------------------------------------------

#' filter_duplicates
#'
#' @param x data.table with time and value columns
#' @param fun first, last, mean, median
#' @param ... arguments to pass to fun
#'
#' @return
#' @export
#'
#' @examples
#' x <- data.table(datetime = sample(1:1000, 10000, replace = TRUE))
#' x[, value := rnorm(nrow(x))]
#' setkey(x, datetime)
#' system.time(filter_duplicates(x, mean))
filter_duplicates <- function(x, fun = data.table::first, ...) {

  # unique
  x <- unique(x)

  # compare times
  n  <- uniqueN(x$datetime)
  nr <- nrow(x)

  # there are no duplicate times
  if(n == nr) {
    return(x)
  }

  message(paste0('There are ', nr - n, ' duplicate values' ))

  x[, fun(value, ...), by = 'datetime']

}


# find_gaps ---------------------------------------------------------------
find_gaps <- function(x) {


  # calculate differences
  x[, dt := as.numeric(datetime) - data.table::shift(as.numeric(datetime), 1)]


  # get typical sample spacing
  n_dt <- x[,.N, by = dt]
  common_dt <- n_dt[which.max(N)][['dt']]


  # get gaps
  gaps <- x[rep(which(dt != common_dt), each = 2L) + c(-1L, 0L)]
  gaps[, grp := rep(1:(.N / 2L), each = 2L)]
  gaps <- gaps[, list(start = data.table::first(datetime),
              end   = data.table::last(datetime)),
       by = grp]


  # calculate midpoint
  gaps[, midpoint := start + (as.numeric(end) - as.numeric(start)) / 2.0]
  gaps[, dt := common_dt]

  x[, dt := NULL]

  return(gaps)

}

make_regular <- function(x, gaps) {

  x <- rbind(x, add_na(gaps))
  setkey(x, datetime)

  invisible(x)

}

add_na <- function(gaps) {

  gaps <- gaps[, list(datetime = seq.POSIXt(start, end, dt),
                  value = NA_real_), by = grp]
  gaps <- gaps[, .SD[-c(1, .N)], by = grp]
  gaps[, grp := NULL]

  gaps
}


filter_changes <- function(x, mult = 100) {

  x[, dp := abs(as.numeric(value) - data.table::shift(as.numeric(value), 1))]
  mn <- mean(x[['dp']], na.rm = TRUE)
  std_dev <- sd(x[['dp']], na.rm = TRUE) * mult
  x[dp > (mn + std_dev), value := NA_real_]

  x

}


add_na_buffers <- function(x, gaps, buffer = c(-1000, 1000)) {

  gaps <- gaps[, start := start + buffer[1]]
  gaps <- gaps[, end := end + buffer[2]]

  print(gaps)
  x[datetime %inrange% gaps[, list(start, end)], value := NA_real_]

  x
}



# x <- filter_duplicates(x)
#
# g <- find_gaps(x)
# x <- make_regular(x, g)
#
# x <- filter_duplicates(x, fun = data.table::first)
#
# x <- filter_changes(x, mult = 50)
# rle(is.na(x$value))
#
# x <- add_na_buffers(x, gaps, buffer = c(-600, 600))
#
# tmp <- x[as.numeric(datetime) %% 600 == 0]
#
#
#
# plot(value~datetime, tmp[value > 10], type='l')
#
#
#
#
#
#
# library(plotly)
# '/media/jonathankennel/Seagate Expansion Drive/rbr_g360/5a 082215_20201008_1210.rsk'
# fn <- list.files('/media/jonathankennel/Seagate Expansion Drive/rbr_g360/',
#                  full.names = TRUE)
# fn <- fn[grep('20201008', fn)]
# wl <- read_transducer(fn)
#
# d <- c(35.83,34.14,28.40,23.23)
# a1 <- 11.6
# a2 <- d-a1
#
# temp <- wl[channel == 'temp12']
# wl <- wl[parameter == 'pressure']
# tmp <- wl[, data[[1]], by = list(name = basename(file))]
# # tmp <- temp[, data[[1]], by = list(name = basename(file))]
# tmp <- tmp[as.numeric(datetime) %% 60 == 0]
#
# tmp[grep('2a', name), value := value - a2[4]]
# tmp[grep('2b', name), value := value - a2[4]]
#
# tmp[grep('3a', name), value := value - a2[3]]
# tmp[grep('3b', name), value := value - a2[3]]
#
# tmp[grep('4a', name), value := value - a2[2]]
# tmp[grep('4b', name), value := value - a2[2]]
#
# tmp[grep('5a', name), value := value - a2[1]]
# tmp[grep('5b', name), value := value - a2[1]]
#
# p1 <- plot_ly(tmp[!grepl('barro', name)],
#         x = ~datetime, y = ~value,
#         type = 'scatter',
#         mode = 'lines',
#         color = ~name)
# p2 <- plot_ly(tmp[grepl('barro', name)],
#               x = ~datetime, y = ~value,
#               type = 'scatter',
#               mode = 'lines',
#               color = ~name)
# subplot(p1, p2, nrows = 2, shareX = TRUE)
# # test of interpolation
# library(rollRegres)
# library(data.table)
#
# xy <- data.table(y = rnorm(3e6),
#            x = rnorm(3e6))
# xy[1e6, x := NA]
# tmp <- rollRegres::roll_regres(y~x, xy, width = 10000, min_obs = 9000)
# sum(is.na(tmp$coefs))
#
# # simulate data
# set.seed(96235555)
# n   <- 10L * 12L * 21L # x years w/ 12 months of 21 trading days
# month <- (seq_len(n) - 1L) %/% 21L + 1L # group by months
# set.seed(29478439)
# X <- matrix(rnorm(n * 4L), ncol = 4L)
# y <- rnorm(n)
#
# # randomly drop rows
# keep <- seq_along(y) %in% sample.int(nrow(X), as.integer(n * .5))
# X     <- X    [keep, ]
# y     <- y    [keep]
# month <- month[keep]
#
# pck_out <- roll_regres.fit(
#   X, y, do_downdates = FALSE,
#   do_compute = c("sigmas", "r.squareds"))
