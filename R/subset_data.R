#===============================================================================
#' @title filter_dates
#'
#' @description remove values from a data.table (non-equi join filter)
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param all data.table to filter (name, datetime)
#' @param subsets data.table of filter start and end times (start, end)
#' @param keep include or exclude the subsets
#' @param include_filt_cols include the columns in the filter table when keep is TRUE
#'
#' @return filtered data.table
#'
#' @export
#===============================================================================
filter_dates <- function(all, subsets,
                         keep = FALSE,
                         include_filt_cols = FALSE) {

  dat <- copy(all)
  filt <- copy(subsets)

  # filter with or without name column
  if (!'name' %in% names(filt) |
      !'name' %in% names(dat)) {

    setkeyv(filt, c("start", "end"))
    key_nm <- key(filt)
    dat_key <- dat[, list(start = datetime, end = datetime)]
    rem_col <- NULL

  } else {

    setkeyv(filt, c("name", "start", "end"))
    key_nm <- key(filt)
    dat_key <- dat[, list(name, start = datetime, end = datetime)]

    rem_col <- "name"

  }

  setkeyv(dat_key, key_nm)

  # print(str(filt))
  # print(str(dat_key))
  # print(key_nm)

  # match intervals
  inds <- na.omit(foverlaps(dat_key,
                            filt,
                            which = TRUE,
                            type = "within"), "yid")

  # return the data.table inside the filter ranges
  if (keep) {

    # return the data.table inside the filter ranges with filter data
    if (include_filt_cols) {


      # which filter group
      filt[, id := 1:nrow(filt)]
      out <- dat[inds$xid][, id := inds$yid]

      setkeyv(out, 'id')
      setkeyv(filt, 'id')

      #out <- out[filt[, -c(rem_col), with = FALSE], nomatch = 0L]
      out <- out[filt, nomatch = 0L]
      #out <- out[, -c('id'), with = FALSE]
      return(out)

    } else {

      return(dat[unique(inds$xid)])

    }


    # return the data.table outside of the filter ranges
  } else {

    return(dat[!unique(inds$xid)])

  }

}

