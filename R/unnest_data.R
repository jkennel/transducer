#' unnest_data
#' This function pulls the data and auxilary columns out of a nested data.table
#'
#' @param x the nested data.table of values
#' @param columns the column names to include with the data
#'
#' @return
#' @export
#'
unnest_data <- function(x, columns = c('serial', 'type', 'is_baro')) {

  x[, rbindlist(data), by = mget(columns)]

}
