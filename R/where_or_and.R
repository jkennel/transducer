#' where_or_and
#'
#' @param sql_text sql text string
#'
#' @return
#' @export
#'
where_or_and <- function(sql_text) {

  if (grepl('WHERE', sql_text)){
    return(' AND ')
  } else {
    return(' WHERE ')
  }

}
