
#' parse_paths
#'
#' Get a vector of file names from a path
#'
#' @param folder_path the folder to export files to
#'
#' @return list of file names
#' @export
#'
#' @examples
#' pp <- c('/media/kennel/Seagate Expansion Drive/rbr_g360/', '/media/kennel/Seagate Expansion Drive/rbr_ssfl/077611_20140827_1025.rsk')
#' pp <- parse_paths(pp, full.names = TRUE, pattern = '.rsk')
parse_paths <- function(folder_path, ...) {

  fn <- list()

  for(i in seq_along(folder_path)) {
    fp <- folder_path[i]

    if(dir.exists(fp)) {
      fn[[i]] <- list.files(fp, ...)
    } else if(file.exists(fp)) {
      fn[[i]] <- fp
    }

  }

  unlist(fn)

}

#' summarize_files
#'
#' This function reads in a folder of rbr files and generates a summary
#'
#' @param file_names the folder to read
#' @param output_folder the folder to export files to
#'
#' @return
#' @export
#'
#' @examples
#' folder_path <- '/media/kennel/Seagate Expansion Drive/rbr_ssfl'
#' tmp <- summarize_files(folder_path)
summarize_files <- function(file_names, output_folder = '') {

  fn <- file_names
 # fnn <- fn[1]
 # fnn <- fn[1]
 #    rbr <- rbindlist(lapply(fn, function(x) {
 #      db <- DBI::dbConnect(RSQLite::SQLite(), x)
 #      print(x)
 #      # get database info
 #      info <- rbr_info(db, x)
 #      info <- rbr_start_end(info)
 #      RSQLite::dbDisconnect(db)
 #      info
 #    }))
 # #
 #    rbr2 <- rbindlist(lapply(fn[273], function(x) {
 #
 #      db <- DBI::dbConnect(RSQLite::SQLite(), x)
 #      print(x)
 #      # get database info
 #      info <- rbr_info(db, x)
 #      info <- rbr_start_end(info)
 #      RSQLite::dbDisconnect(db)
 #      info
 #
 #    }))
  rbr <- rbindlist(lapply(fn, function(nm) {
    print(nm)
    rbr_sub <- read_transducer(nm)

    if(nrow(rbr_sub) >= 1){

      data_sub <- lapply(rbr_sub$data, function(x) {

        x[, list(median = median(value),
                 mean = mean(value),
                 max = max(value),
                 min = min(value),
                 sd = sd(value),
                 n = .N),
          by = list(datetime = as.POSIXct(as.numeric(datetime) %/% (3600) * (3600),
                                          origin = '1970-01-01'))]

      })
      rbr_sub[, data := data_sub]
    }

    out_name <- gsub('.rsk', '', basename(rbr_sub[1]$file))
    print(paste0(output_folder, out_name, '.rds'))
    saveRDS(rbr_sub, paste0(output_folder, out_name, '.rds'))
    rbr_sub

  }))

  rbr
}







