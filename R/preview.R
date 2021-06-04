
#' @title hydrograph
#'
#' create a hydrograph
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
hydrograph <- function (x, ...) {
  UseMethod("hydrograph", x)
}


#' #' hydrograph
#' #'
#' #' @param x
#' #' @param type
#' #' @param max_n
#' #'
#' #' @importFrom plotly plot_ly layout
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' hydrograph.character <- function(x,
#'                               var_type = 'pressure',
#'                               by = 3600,
#'                               start = NULL,
#'                               end = NULL) {
#'
#'   if (is.null(start) & is.null(end)) {
#'     int   <- rbr_start_end(x)
#'     start <- min(int$start)
#'     end   <- max(int$end)
#'   }
#'
#'   times <- seq.int(as.numeric(start), as.numeric(end), by = by)
#'
#'   x <- read_rbr(x, times = times)
#'   x <- x[type == var_type]
#'
#'   x
#'   #preview(x, var_type = 'pressure', times = times, plot = plot, plot_col = plot_col)
#'
#' }

#' @title hydrograph
#'
#' create a hydrograph
#'
#' @param x
#' @param var_type
#' @param max_n
#' @param plot
#'
#' @importFrom plotly plot_ly layout subplot
#' @return
#' @export
#'
hydrograph.data.table <- function(x,
                               var_type = 'pressure',
                               plot_col = 'value',
                               max_n = 10000) {

  x_long <-   unnest_data(x[is_baro == FALSE  & type == var_type], columns = 'serial')
  y_long <-   unnest_data(x[is_baro == TRUE  & type == var_type], columns = 'serial')


  p1 <- plotly::plot_ly(x_long,
                        x = ~datetime, y = ~get(plot_col),
                        type='scatter',
                        mode = 'lines',
                        name = ~serial)
  p2 <- plotly::plot_ly(y_long,
                        x = ~datetime, y = ~value,
                        type='scatter',
                        mode = 'lines',
                        name = ~serial)
  p2 <- layout(p2, yaxis = list(range = c(9.3, 10.2)))

  subplot(p1, p2, nrows = 2, shareX = TRUE, heights = c(0.7, 0.3))

}






#' @title vertigraph
#'
#' Creates a vertical profile
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
vertigraph <- function (x, ...) {
  UseMethod("vertigraph", x)
}


#' #' vertigraph
#' #'
#' #' @param x
#' #' @param type
#' #' @param max_n
#' #'
#' #' @importFrom plotly plot_ly layout
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' vertigraph.character <- function(x,
#'                               var_type = 'pressure',
#'                               by = 3600,
#'                               start = NULL,
#'                               end = NULL) {
#'
#'   if (is.null(start) & is.null(end)) {
#'     int   <- rbr_start_end(x)
#'     start <- min(int$start)
#'     end   <- max(int$end)
#'   }
#'
#'   times <- seq.int(as.numeric(start), as.numeric(end), by = by)
#'
#'   x <- read_rbr(x, times = times)
#'   x <- x[type == var_type]
#'
#'   x
#'   #preview(x, var_type = 'pressure', times = times, plot = plot, plot_col = plot_col)
#'
#' }

#' @title vertigraph
#'
#' Creates a vertical profile
#'
#' @param x
#' @param var_type
#' @param max_n
#' @param plot
#'
#' @importFrom plotly plot_ly layout subplot
#' @return
#' @export
#'
#' @examples
vertigraph.data.table <- function(x,
                                  var_type = 'pressure',
                                  plot_col = 'value',
                                  max_n = 10000) {



  x_long <- unnest_data(x[is_baro == FALSE  & type == var_type], columns = c('serial', 'elevation'))

  n <- length(unique(x_long$datetime))

  p2 <- plotly::plot_ly(x_long, y = ~elevation, x = ~get(plot_col),
                color = ~as.character(datetime),
                colors = viridis(n),
                type = 'scatter',
                mode = 'lines+markers',
                marker = list(size = 10))
  p2 <- plotly::add_trace(p2, color = I("black"), frame = ~datetime)
  p2 <- plotly::layout(p2,
      xaxis = list(title='Head (masl)',
                   range = range(x_long[[plot_col]])+c(-0.5,0.5)),
      yaxis = list(title='Elevation (masl)',
                   range = range(x_long$elevation)+c(-2,2)))

  p2 <- plotly::hide_legend(p2)
  p2 <- plotly::animation_opts(p2, transition=0, easing='linear')
  p2 <- plotly::animation_slider(p2, currentvalue = list(prefix = "", font = list(color = "steelblue")))
  return(p2)
}


# subset_plot <- function(x, ylab, serial) {
#
#   ax <- list(title = '')
#   ay <- list(title = ylab)
#
#   plotly::plot_ly(x, x = ~datetime, y = ~value, name = ~serial,
#           type = 'scatter', mode = 'lines') %>%
#     plotly::layout(xaxis = ax, yaxis = ay)
#
# }



# plot_hydrograph <- function() {
#
#
#
#   p1 <- plotly::plot_ly(x_long,
#                         x = ~datetime, y = ~get(plot_col),
#                         type='scatter',
#                         mode = 'lines',
#                         name = ~serial)
#   p2 <- plotly::plot_ly(y_long,
#                         x = ~datetime, y = ~value,
#                         type='scatter',
#                         mode = 'lines',
#                         name = ~serial) %>%
#     layout(yaxis = list(range = c(9.3, 10.2)))
#
#   print(subplot(p1, p2, nrows = 2, shareX = TRUE, heights = c(0.7, 0.3)))
#
# }


# locations <- fread('/media/kennel/Data/phd/personnel/pat/syngenta/transducer_depth_path.csv')
# locations[, file_name := tail(strsplit(path, '/')[[1]], 1), by = 1:nrow(locations)]
# locations <- locations[, list(well, port = id, elevation = 300-depth, is_baro = baro, file_name, serial)]
#
# db_name <- '/media/kennel/Data/phd/personnel/pat/syngenta/SYN UW1 P1-07 - 077924_20150413_1746.rsk'
# system.time(wl <- preview(db_name, location = locations, by = 3600, plot = TRUE))

# db_name <- list.files( '/media/kennel/Data/phd/personnel/pat/syngenta/',
#                        full.names = TRUE,
#                        pattern = '*.rsk')
#
# locations <- fread('/media/kennel/Data/phd/personnel/pat/syngenta/transducer_depth_path.csv')
# locations[, file_name := tail(strsplit(path, '/')[[1]], 1), by = 1:nrow(locations)]
# locations <- locations[, list(well, port = id, elevation = (309.33 + 0.319)-depth, is_baro = baro, file = path)]
#
# wl <- read_rbr(locations, start = as.POSIXct('2014-12-15', tz = 'UTC'),
#                     end   = as.POSIXct('2015-04-01', tz = 'UTC'),
#          by = 3600)

# system.time(
#   x <- read_rbr(db_name, by = 3600*3)
# )


# system.time(
#   x <- read_rbr(db_name,
#                 start = as.POSIXct('2014-12-15', tz = 'UTC'),
#                 end   = as.POSIXct('2015-04-01', tz = 'UTC'), by = 3600)
# )


# times <- seq.int(as.POSIXct('2014-12-15', tz = 'UTC'),
#                  as.POSIXct('2015-04-01', tz = 'UTC'),
#                  by = 3600)
#
# wl <- read_rbr(db_name, times = times) %>%
#   add_location()



