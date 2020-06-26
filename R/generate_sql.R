#' generate_sql
#'
#' @inheritParams read_rbr
#' @param db database connection
#'
#' @return sql string
#' @export
#'
#' @importFrom dbplyr remote_query
#' @importFrom dplyr tbl_vars
#' @examples
#' generate_sql(Sys.time()-1000, Sys.time(), 10)
#'
#'
# generate_sql <- function(db, start = NULL, end = NULL,
#          n_skip = NULL, max_n = NULL) {
#
#
#   # get column names
#   col_names <- dplyr::tbl_vars(dplyr::tbl(db, 'data'))
#
#   if (!is.null(start)) {
#     start_id <- paste0("SELECT rowid FROM data WHERE tstamp > ", as.numeric(start)*1000 ," LIMIT 1")
#     start_id <- as.integer(collect(dplyr::tbl(db, dplyr::sql(start_id)))$rowid)
#   } else {
#     start_id <- paste0("SELECT rowid FROM data LIMIT 1")
#     start_id <- as.integer(collect(dplyr::tbl(db, dplyr::sql(start_id)))$rowid)
#   }
#   if (!is.null(end)) {
#     end_id <- paste0("SELECT rowid FROM data WHERE tstamp < ",
#                      as.numeric(end) * 1000 ," ORDER BY rowid DESC LIMIT 1")
#     end_id <- as.integer(collect(dplyr::tbl(db, dplyr::sql(end_id)))$rowid)
#   } else {
#     end_id <- paste0("SELECT rowid FROM data ORDER BY rowid DESC LIMIT 1")
#     end_id <- as.integer(collect(dplyr::tbl(db, dplyr::sql(end_id)))$rowid)
#   }
#
#   # number of values
#   n <- end_id - start_id + 1
#
#
#   if(!is.null(max_n)) {
#     if (max_n >= n) {
#       max_n <- NULL
#     }
#   }
#
#   if(!is.null(n_skip)) {
#     if (n_skip == 1) {
#       n_skip <- NULL
#     }
#   }
#
#
#   # if no subsetting is desired
#   if (is.null(start) & is.null(end) & is.null(n_skip) & is.null(max_n)) {
#     # return entire table
#     nm <- c('`tstamp` / 1000 AS datetime', paste0('`', col_names[-1], '`'))
#     return(paste0('SELECT ', paste(nm, collapse = ', '), ' FROM data'))
#   }
#
#
#   col_names <- c('datetime', col_names[-1])
#   col_names <- paste0('`', col_names, '`')
#   sql_sel   <- paste0('SELECT ', paste0(col_names, collapse = ', '), ' FROM (')
#
#   # do skipping
#   if (!is.null(max_n)) {
#     n_skip <- n %/% max_n
#   }
#
#   # print(n_skip)
#   if (!is.null(n_skip)) {
#     if (n_skip < n) {
#       if ((n %/% n_skip) < 1e5) {
#         inds <- seq.int(start_id, end_id, by = n_skip)
#         print(inds)
#         sql_skip <- paste0(") WHERE `rowid` IN (", paste(inds, collapse = ','), ")")
#       } else {
#         mod_start <- start_id %% n_skip
#         sql_skip <- paste0(") WHERE `rowid` % ", n_skip, " = ", mod_start)
#       }
#     }
#   } else {
#     sql_skip <- ')'
#   }
#
#   if(!is.null(start) | !is.null(end)) {
#
#     s <- dplyr::tbl(db, 'data') %>%
#       mutate(datetime = tstamp / 1000) %>%
#       filter(between('rowid', start_id, end_id)) %>%
#       dbplyr::remote_query() %>%
#       as.character()
#   } else {
#     s <- dplyr::tbl(db, 'data') %>%
#       mutate(datetime = tstamp / 1000) %>%
#       dbplyr::remote_query() %>%
#       as.character()
#   }
#
#   # remove tstamp column and carriage return
#   s <- gsub('\n',  ' ', s)
#   s <- gsub("'rowid'",  '\`rowid\`', s)
#   s <- gsub(' \`tstamp\`,',  ' \`rowid\`,', s)
#
#
#   # print(s)
#   # print(sql_skip)
#   # print(sql_sel)
#   paste0(sql_sel, s, sql_skip)
#
# }



#' generate_sql
#'
#' @inheritParams read_rbr
#'
#' @return sql string
#' @export
#'
#' @importFrom dbplyr remote_query
#' @importFrom dplyr tbl_vars
#' @examples
#'
#'
#'
generate_sql <- function(db, start = NULL, end = NULL, by = NULL) {


  nm_tbl <- RSQLite::dbListTables(db)
  if (!'data' %in% nm_tbl) {
    return('')
  }

  # get column names
  col_names <- DBI::dbListFields(db, 'data')

  # remove datasetID column if present
  col_names <- col_names[!grepl('datasetID', col_names)]

  #nm <- c(paste0('`', col_names, '`'))
  sql_base <- paste0('SELECT ', paste(col_names, collapse = ', '),
                     ' FROM data')

  # no subset
  if (is.null(start) & is.null(end) & is.null(by)) {
    return(sql_base)
  }

  # subset dates
  if (!is.null(start) & !is.null(end) & is.null(by)) {
    return(paste0(sql_base, ' WHERE tstamp BETWEEN ', as.numeric(start) * 1000, ' AND ', as.numeric(end) * 1000))
  }

  # get start and end
  # this speeds up short time period subsetting
  if (is.null(start)) {
    start_id <- paste0("SELECT tstamp FROM data ORDER BY tstamp LIMIT 1")
  } else {
    start_id <- paste0("SELECT tstamp FROM data WHERE tstamp >= ", as.numeric(start) * 1000, " ORDER BY tstamp LIMIT 1")
  }
  if(is.null(end)) {
    end_id <- paste0("SELECT tstamp FROM data ORDER BY tstamp DESC LIMIT 1")
  } else {
    end_id <- paste0("SELECT tstamp FROM data WHERE tstamp <= ", as.numeric(end) * 1000, " ORDER BY tstamp DESC LIMIT 1")
  }

  start_id <- as.numeric(RSQLite::dbGetQuery(db, start_id)$tstamp)
  end_id   <- as.numeric(RSQLite::dbGetQuery(db, end_id)$tstamp)

  n <- (end_id - start_id) %/% (by * 1000)

  # do first sub
  if (!is.null(start) | !is.null(end)){
    sql_str <- paste0('SELECT * FROM (',
                      sql_base,
                      ' WHERE tstamp BETWEEN ', start_id, ' AND ', end_id,
                      ') WHERE tstamp % ', by * 1000, ' = 0')

  } else {
    sql_str <- paste0(sql_base, paste0(' WHERE tstamp % ', by * 1000, ' = 0'))
  }

  return(sql_str)

}


#' generate_sql_times
#'
#' @param start
#' @param end
#' @param by
#' @param times
#'
#' @return
#' @export
#'
#' @examples
generate_sql_times <- function(start = NULL, end = NULL, by = NULL, times = NULL) {

  # read data
  if (!is.null(start) & !is.null(end) & !is.null(by)) {

    if((end-start) / by > 1e5) {
      return("")
    }

    times <- seq.int(as.numeric(start), as.numeric(end), by = by)
  }

  # set times
  if(!is.null(times)) {
    if (length(times) > 1e5) {
      return("")
    }
    return(paste0(' WHERE tstamp IN (', paste(times*1000, collapse = ', '), ')'))
  }



  return("")
}



# generate_sql <- function(start = NULL, end = NULL, by = NULL) {
#
#
#   sql_base <- 'SELECT * FROM data'
#
#   if (sum(c(is.null(start), is.null(end))) == 1) {
#     stop('Currently both start and end must be null, or both have values')
#   }
#
#   # no subset
#   if (is.null(start) & is.null(end) & is.null(by)) {
#     return(sql_base)
#   }
#
#   # date interval
#   if (!is.null(start) & !is.null(end) & is.null(by)) {
#     return(paste0(sql_base, ' WHERE tstamp BETWEEN ', as.numeric(start) * 1000, ' AND ', as.numeric(end) * 1000))
#   }
#
#   # date interval
#   if (is.null(start) & is.null(end) & !is.null(by)) {
#     return(paste0(sql_base, ' WHERE tstamp % ', by * 1000, ' = 0'))
#   }
#
#
#   # date interval and subset
#   n <- (as.numeric(end) - as.numeric(start)) / by
#   if (n > 1e5) {
#     sql_str <- paste0('SELECT * FROM (',
#                       sql_base,
#                       ' WHERE tstamp BETWEEN ', as.numeric(start) * 1000, ' AND ', as.numeric(end) * 1000,
#                       ') WHERE tstamp % ', by * 1000, ' = 0')
#   } else {
#     times <- seq.int(as.numeric(start) * 1000,
#                      as.numeric(end) * 1000,
#                      by = by * 1000)
#     sql_str <- paste0(sql_base,
#                       paste0(' WHERE tstamp IN (', paste(times, collapse = ', '), ')'))
#   }
#
#
#   return(sql_str)
#
# }





#
#   # do second subset
#   if(!is.null(start) & !is.null(end) & !is.null(by)) {
#
#     times <- seq.POSIXt(start, end, by = by)
#     n <- length(times)
#
#     if (n > 1e5) {
#       sql_str <- paste0('SELECT * FROM (',
#                         sql_str,
#                         paste0(' ORDER BY tstamp) WHERE tstamp % ', by * 1000, ' = 0'))
#     } else {
#       sql_str <- paste0(
#         sql_base,
#         paste0(' WHERE tstamp IN (', paste(as.numeric(times) * 1000, collapse = ', '), ')'))
#     }
#   }
#
#
#   sql_str




#
#   e <- s %>% arrange(desc(tstamp)) %>% head(1) %>% collect()
#   s <- s %>% head(1) %>% collect()
#
#
#
#   e <- dplyr::tbl(db, 'data') %>%
#     mutate(datetime = tstamp / 1000) %>%
#     filter(datetime < as.numeric(end)) %>%
#     arrange(desc(tstamp)) %>%
#     select(-tstamp)
#
#
#   system.time(
#     col_names <- names(collect(dplyr::tbl(db, dplyr::sql("SELECT * FROM data LIMIT 1"))))
#     col_names <- col_names[-1]
#     col_names
#   )
#
#
#   # work with rowid for speed
#   if (!is.null(start)) {
#
#     s <- paste0("SELECT rowid as n FROM (",
#                 "SELECT rowid FROM data WHERE tstamp >= ", as.numeric(start) * 1000.0,
#                 ") LIMIT 1")
#
#     s <- collect(dplyr::tbl(db, dplyr::sql(s)))$n
#
#   } else {
#     s <- 1
#   }
#
#   if (!is.null(end)) {
#
#     e <- paste0("SELECT rowid as n FROM (",
#                 "SELECT rowid FROM data WHERE tstamp <= ", as.numeric(end) * 1000.0,
#                 ") ORDER BY rowid DESC LIMIT 1")
#
#     e <- collect(dplyr::tbl(db, dplyr::sql(e)))$n
#
#   } else {
#     e <- paste0("SELECT rowid as n FROM (SELECT rowid FROM data) ORDER BY rowid DESC LIMIT 1")
#     e <- as.data.table(dplyr::tbl(db, dplyr::sql(e)))
#
#   }
#
#
#
#   sql_text <- sql_subset(s, e, n_skip = n_skip, max_n = max_n)
#
#   sql_text
# }



# sql_subset <- function(s, e, n_skip = NULL, max_n = NULL) {
#
#   sql_text <- "SELECT rowid, tstamp/1000.0 as datetime, * FROM data"
#
#   n <- e - s + 1
#
#   if (!is.null(n_skip)) {
#     if (n_skip > n) {
#       return(sql_text)
#     }
#     max_n <- n %/% n_skip
#   } else if (!is.null(max_n)) {
#     if (max_n > n) {
#       return(sql_text)
#     }
#     n_skip <- n %/% max_n
#   }
#
#   if (!is.null(max_n)) {
#     if (max_n > 1e5) {
#       sql_inner <- paste0(sql_text,
#                          where_or_and(sql_text),
#                          "rowid % ", n_skip, " = 1")
#     } else {
#       inds <- seq.int(s, e, by = n_skip)
#       sql_inner <- paste0(sql_text,
#                          where_or_and(sql_text),
#                          "rowid IN (", paste(inds, collapse = ','), ")")
#     }
#   }
#
#   sql_middle <- paste0("SELECT rowid as n FROM (",
#                        sql_inner,
#                        ") WHERE rowid BETWEEN ",
#                          s-1, " AND ", e+1)
#
#
#
#
#
#
#
#   # filter by date
#   if (is.null(n_skip) & is.null(max_n)) {
#     if (!is.null(s) & !is.null(e)) {
#
#       sql_text <- paste0("SELECT tstamp/1000.0 as datetime, * FROM data WHERE rowid BETWEEN",
#                          s-1, " AND ", e+1)
#     } else if (!is.null(s)) {
#       sql_text <- paste0("SELECT tstamp/1000.0 as datetime, * FROM data WHERE rowid > ",
#                          s-1)
#     } else {
#       sql_text <- paste0("SELECT tstamp/1000.0 as datetime, * FROM data WHERE rowid < ",
#                          e)
#     }
#
#     return(sql_text)
#   }
#
#
#   if (is.null(start) & is.null(end)) {
#     sql_text <- "SELECT tstamp/1000.0 as datetime, * FROM data"
#
#     n <- e - s + 1
#
#     if (!is.null(n_skip)) {
#       if (n_skip > n) {
#         return(sql_text)
#       }
#       max_n <- n %/% n_skip
#     } else if (!is.null(max_n)) {
#       if (max_n > n) {
#         return(sql_text)
#       }
#       n_skip <- n %/% max_n
#     }
#
#     if (!is.null(max_n)) {
#       if (max_n > 1e5) {
#         sql_text <- paste0(sql_text,
#                            where_or_and(sql_text),
#                            "rowid % ", n_skip, " = 1")
#       } else {
#         inds <- seq.int(s, e, by = n_skip)
#         sql_text <- paste0(sql_text,
#                            where_or_and(sql_text),
#                            "rowid IN (", paste(inds, collapse = ','), ")")
#       }
#     }
#   }
#
#   sql_text
#
# }
#
#
# s <- paste0("SELECT rowid as n FROM (",
#                              "SELECT rowid FROM data WHERE tstamp >= ", as.numeric(start) * 1000.0,
#                              ") WHERE rowid > 5000")
#
#       s <- collect(dplyr::tbl(db, dplyr::sql(s)))

